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
  ) where

import           Control.Monad.State.Strict (StateT, MonadState)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Except (MonadError, Except)
import qualified Control.Monad.Except as Except
import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map


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

data OpCode
  = Halt
  | Add
  | Mult
  | Input
  | Output
  deriving (Show)

data ParameterMode
  = PositionMode
  | ImmediateMode
  deriving (Show, Eq)

parameterModeFromInt :: MonadError String m => Int -> m ParameterMode
parameterModeFromInt 0 = pure PositionMode
parameterModeFromInt 1 = pure ImmediateMode
parameterModeFromInt m = Except.throwError $ "invalid parameter-mode " ++ show m


data Instruction = Instruction
  { opCode        :: OpCode
  , parameters    :: [Parameter]
  } deriving (Show)


data Parameter = Parameter
  { parameterMode  :: ParameterMode
  , parameterValue :: Int
  } deriving (Show)


-- | determins the length of an instruction
--   so that we can move the instruction-pointer to the next one
instructionLength :: Instruction -> Int
instructionLength Instruction { opCode = _, parameters = ps } = 1 + length ps


----------------------------------------------------------------------
-- Interpretation of Programms

-- | using a stack of an state-monad containing the 'Program' and
--   Except String vor error messages
type IntCodeM m = (MonadError String m, MonadState Computer m)

-- | evaluates an action in our 'IntCodeM' monad using the given program
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


-- | runs a single instruction according to the specifications of Day2 cc
runInstruction :: IntCodeM m => Instruction -> m ()
runInstruction Instruction{ opCode = Halt } = 
  -- no-op on halt
  pure ()
runInstruction Instruction{ opCode = Add, parameters = [locA, locB, Parameter PositionMode locTgt] } = do
  -- add the value in the addresses given by the first 2 parameters and
  -- write the result to the third
  operandA <- readParameter locA
  operandB <- readParameter locB
  setMemory locTgt (operandA + operandB)
  moveNext
runInstruction Instruction{ opCode = Mult, parameters = [locA, locB, Parameter PositionMode locTgt] } = do
  -- multiply the value in the addresses given by the first 2 parameters and
  -- write the result to the third
  operandA <- readParameter locA
  operandB <- readParameter locB
  setMemory locTgt (operandA * operandB)
  moveNext
runInstruction Instruction{ opCode = Input, parameters = [Parameter PositionMode toLoc] } = do
  inp <- readInput
  setMemory toLoc inp
  moveNext
runInstruction Instruction{ opCode = Output, parameters = [from] } = do
  out <- readParameter from
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
    


-- | reads the memory value at the given address
readParameter :: IntCodeM m => Parameter -> m Int
readParameter (Parameter ImmediateMode v) = pure v
readParameter (Parameter PositionMode adr) = getMemory adr


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


readInput :: IntCodeM m => m Int
readInput = do
  inputs <- State.gets inputStore
  case inputs of
    [] -> Except.throwError "no input available"
    inp:restInput -> do
      State.modify (\s -> s { inputStore = restInput })
      pure inp


writeOutput :: MonadState Computer m => Int -> m ()
writeOutput out = 
  State.modify (\s -> s { outputStore = out : outputStore s })


getOutputs :: MonadState Computer m => m [Int]
getOutputs = State.gets (reverse . outputStore)

-- | gets the current instruction pointer
getInstructionPointer :: MonadState Computer m => m Address
getInstructionPointer = State.gets instructionPointer


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