{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module IntCode 
  ( Program, Computer
  , Address
  , parseProgram
  , eval
  , runComputer
  , getMemory
  , setMemory
  , getOutputs
  , showProgram
  ) where

import           Control.Monad (forM_)
import           Control.Monad.State.Strict (StateT, MonadState)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Except (MonadError, Except)
import qualified Control.Monad.Except as Except
import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import           Data.List (intercalate)


type Address = Int
type Memory = IntMap Int
type Program = [Int]

data Computer = Computer
  { memory             :: Memory
  , instructionPointer :: Address
  , inputStore         :: [Int]
  , outputStore        :: [Int]
  } deriving (Show)

----------------------------------------------------------------------
-- OpCode and Instructions

-- | a parsed instruction for our little computer
--   the op-code determins the action 
--   and the parameters are the input for this action 
data Instruction = Instruction
  { opCode        :: OpCode 
  , parameters    :: [Parameter]
  } deriving (Show)


-- | our computers command-set
data OpCode
  = Halt
  | Add
  | Mult
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  deriving (Show)


-- | a parameter for an instruction
--   each parameter has a mode and a value
--   the mode flags if the value is to be interpreted as an address or as an value
data Parameter = Parameter
  { parameterMode  :: ParameterMode
  , parameterValue :: Int
  } deriving (Show)


-- | the position mode determins if a parameter is
--   interpreted as a pointer to a location ('PositionMode')
--   or as an actual constant value ('ImmediateMode')
data ParameterMode
  = PositionMode
  | ImmediateMode
  deriving (Show, Eq)


-- | parses the parameter mode from the numbers given in the problem/description
parameterModeFromInt :: MonadError String m => Int -> m ParameterMode
parameterModeFromInt 0 = pure PositionMode
parameterModeFromInt 1 = pure ImmediateMode
parameterModeFromInt m = Except.throwError $ "invalid parameter-mode " ++ show m


-- | calculates the length of an instruction
--   so that we can move the instruction-pointer to the next one
instructionLength :: Instruction -> Int
instructionLength Instruction { opCode = _, parameters = ps } = 1 + length ps


----------------------------------------------------------------------
-- Interpretation of Programms

-- | using a stack of an state-monad containing the 'Program' and
--   Except String vor error messages
type IntCodeM m = (MonadError String m, MonadState Computer m)

-- | evaluates an action in our 'IntCodeM' monad using the given program
--   the second parameter 'inputs' is stored as the input-stack for the
--   computer (inputs will be pulled from this list left to right)
eval :: Program -> [Int] -> StateT Computer (Except String) a -> Either String a
eval prg inputs act = 
  let computer = Computer (Map.fromList $ zip [0..] prg) 0 inputs []
  in Except.runExcept $ State.evalStateT act computer 


-- | keeps running instructions until halt
runComputer :: IntCodeM m => m ()
runComputer = do
  instr <- getCurrentInstruction
  case opCode instr of
    Halt -> pure ()
    _ -> runInstruction instr >> runComputer


-- | formats the current program into a string - stops on error
showProgram :: Program -> String
showProgram prg = 
  case eval prg [] parseMemory of
    Left err -> "ERROR: " ++ err
    Right listing -> unlines $ map showInstr listing
  where
    showInstr (Instruction opCode params) =
      show opCode ++ "(" ++ intercalate "," (map showParam params) ++ ")"
    showParam (Parameter PositionMode v) = "&" ++ show v
    showParam (Parameter ImmediateMode v) = show v


-- | parses the current memory configuration into a list of instructions
--   for debugging / reading
parseMemory :: IntCodeM m => m [Instruction]
parseMemory = do
  (lower, upper) <- memoryBounds
  go lower upper
  where
    go current upper | current > upper = pure []
    go current upper = do
      instr <- tryGetInstr current
      case instr of
        Nothing -> pure []
        Just instruction -> do
          let len = instructionLength instruction
          rest <- go (current + len) upper
          pure (instruction : rest)
    tryGetInstr :: IntCodeM m => Address -> m (Maybe Instruction)
    tryGetInstr adr =
      flip Except.catchError (\_ -> pure Nothing) $ Just <$> getInstruction adr


-- | runs a single instruction according to the specifications of Day2 cc
runInstruction :: IntCodeM m => Instruction -> m ()
runInstruction Instruction{ opCode = Halt } = 
  -- no-op on halt
  pure ()
runInstruction Instruction{ opCode = Add, parameters = [locA, locB, Parameter PositionMode locTgt] } = do
  -- add the value in from the first two given parameters and
  -- write the result to the third's address (only valid if it is in 'PositionMode')
  operandA <- evalParameter locA
  operandB <- evalParameter locB
  setMemory locTgt (operandA + operandB)
  moveNext
runInstruction Instruction{ opCode = Mult, parameters = [locA, locB, Parameter PositionMode locTgt] } = do
  -- multiplies the value in from the first two given parameters and
  -- write the result to the third's address (only valid if it is in 'PositionMode')
  operandA <- evalParameter locA
  operandB <- evalParameter locB
  setMemory locTgt (operandA * operandB)
  moveNext
runInstruction Instruction{ opCode = JumpIfTrue, parameters = [cond, loc] } = do
  -- jumps to the address of the second parameter if the value of the first is
  -- non-zero - if not just moves to the next instruction
  condVal <- evalParameter cond
  jumpAdr <- evalParameter loc
  if condVal == 0 
    then moveNext
    else jumpTo jumpAdr
runInstruction Instruction{ opCode = JumpIfFalse, parameters = [cond, loc] } = do
  -- jumps to the address of the second parameter if the value of the first is
  -- zero - if not just moves to the next instruction
  condVal <- evalParameter cond
  jumpAdr <- evalParameter loc
  if condVal == 0 
    then jumpTo jumpAdr
    else moveNext
runInstruction Instruction{ opCode = LessThan, parameters = [a, b, Parameter PositionMode locTgt] } = do
  -- compares parameter-values of the first and second parameter
  -- writes 1 to the address of the last parameter if the first value is less than the second
  -- if not writes 0
  aVal <- evalParameter a
  bVal <- evalParameter b
  let res = if aVal < bVal then 1 else 0
  setMemory locTgt res
  moveNext
runInstruction Instruction{ opCode = Equals, parameters = [a, b, Parameter PositionMode locTgt] } = do
  -- compares parameter-values of the first and second parameter
  -- writes 1 to the address of the last parameter if the first value is equal to the second
  -- if not writes 0
  aVal <- evalParameter a
  bVal <- evalParameter b
  let res = if aVal == bVal then 1 else 0
  setMemory locTgt res
  moveNext
runInstruction Instruction{ opCode = Input, parameters = [Parameter PositionMode toLoc] } = do
  -- reads the next input from the input "stream"
  -- and writes it to the address of the parameter
  inp <- readInput
  setMemory toLoc inp
  moveNext
runInstruction Instruction{ opCode = Output, parameters = [from] } = do
  -- gets the value from the parameter and writes it to the "output" stream
  out <- evalParameter from
  writeOutput out
  moveNext
runInstruction inst =
  -- all other cases should be an invalid instruction (wrong number or parameters for example)
  Except.throwError $ "invalid instruction: '" ++ show inst ++ "'"


-- | gets the instruction at the instruction-pointer
getCurrentInstruction :: IntCodeM m => m Instruction
getCurrentInstruction = do
  adr <- getInstructionPointer
  getInstruction adr


-- | reads a instruction starting at the given memory address
getInstruction :: IntCodeM m => Address -> m Instruction
getInstruction adr = do
  (opCode, modes) <- parseInstrCode <$> getMemory adr
  case opCode of
    99 -> pure $ Instruction Halt []
    1  -> Instruction Add <$> getParams 3 (adr+1) modes
    2  -> Instruction Mult <$> getParams 3 (adr+1) modes
    3  -> Instruction Input <$> getParams 1 (adr+1) modes
    4  -> Instruction Output <$> getParams 1 (adr+1) modes
    5  -> Instruction JumpIfTrue <$> getParams 2 (adr+1) modes
    6  -> Instruction JumpIfFalse <$> getParams 2 (adr+1) modes
    7  -> Instruction LessThan <$> getParams 3 (adr+1) modes
    8  -> Instruction Equals <$> getParams 3 (adr+1) modes
    _ -> Except.throwError $ "unknown opcode " ++ show opCode
  where
    parseInstrCode code =
      let opCode = code `mod` 100
          modes  = code `div` 100
      in (opCode, modes)
    getParams 0 _ _ = pure []
    getParams n adr modes = do
      value <- getMemory adr
      mode <- parameterModeFromInt (modes `mod` 10)
      let param = Parameter mode value
      rest <- getParams (n-1) (adr+1) (modes `div` 10)
      pure (param:rest)
    


-- | evaluates the parameter against the current memory content
--   if it's an 'ImmediateMode' parameter it's just it's value
--   if it's an 'PositionMode' parameter it's the memory content of the address
--   = the parameters value
evalParameter :: IntCodeM m => Parameter -> m Int
evalParameter (Parameter ImmediateMode v) = pure v
evalParameter (Parameter PositionMode adr) = getMemory adr


-- | reads the memory value at the given address
getMemory :: IntCodeM m => Address -> m Int
getMemory adr = do
  found <- State.gets (Map.lookup adr . memory)
  case found of
    Just value -> pure value
    Nothing    -> Except.throwError $ "no value at memory-address " ++ show adr


-- | sets the memory at the given address to a new value
setMemory :: MonadState Computer m => Address -> Int -> m ()
setMemory adr val = 
  State.modify (\prg -> prg { memory = Map.insert adr val (memory prg) })


-- | get's the lowest and higest valid address in memory
memoryBounds :: IntCodeM m => m (Address, Address)
memoryBounds = do
  lower <- State.gets (fmap fst . Map.lookupMin . memory)
  upper <- State.gets (fmap fst . Map.lookupMax . memory)
  let bounds = (,) <$> lower <*> upper
  maybe (Except.throwError "memory is empty") pure bounds


-- | pops the next value from the input-stack and returns it
--   if no more values are on it fails with an error
readInput :: IntCodeM m => m Int
readInput = do
  inputs <- State.gets inputStore
  case inputs of
    [] -> Except.throwError "no input available"
    inp:restInput -> do
      State.modify (\s -> s { inputStore = restInput })
      pure inp


-- | pushes the value to the output-stack
writeOutput :: MonadState Computer m => Int -> m ()
writeOutput out = 
  State.modify (\s -> s { outputStore = out : outputStore s })


-- | returns the current values from the output-stack
--   in "first-in-first-out" order
getOutputs :: MonadState Computer m => m [Int]
getOutputs = State.gets (reverse . outputStore)


-- | gets the current instruction pointer
getInstructionPointer :: MonadState Computer m => m Address
getInstructionPointer = State.gets instructionPointer


-- | jumps to the given address by setting the instruction pointer to it
jumpTo :: IntCodeM m => Address -> m ()
jumpTo = setInstructionPointer 


-- | moves the instruction-pointer to the next instruction after the current
moveNext :: IntCodeM m => m ()
moveNext = do
  inst <- getCurrentInstruction
  moveInstructionPointer (instructionLength inst)


-- | moves the instruction-pointer to the current address increased by the given value
moveInstructionPointer :: MonadState Computer m => Int -> m ()
moveInstructionPointer delta = do
  ip <- getInstructionPointer
  setInstructionPointer (ip + delta)


-- | sets the instruction pointer to the new value
setInstructionPointer :: MonadState Computer m => Int -> m ()
setInstructionPointer ip =
  State.modify (\prg -> prg { instructionPointer = ip })


-- | parses a program from a string
--   assumes the input are just integers separated by ',' characters and
--   possible whitespace
--   tries doing so by rewriting the input as a Haskell list and 'read' this
parseProgram :: String -> Program
parseProgram input =
  read $ "[" ++ input ++ "]"