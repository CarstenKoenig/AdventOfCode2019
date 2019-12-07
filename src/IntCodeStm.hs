{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module IntCodeStm 
  ( Program, Computer
  , Address
  , createComputer
  , parseProgram
  , runComputer
  , getMemory
  , setMemory
  ) where


import qualified Control.Monad.Except as Except
import           Control.Monad.Except (MonadError, ExceptT)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (StateT, MonadState)
import qualified Control.Monad.STM as STM
import           Control.Monad.STM (STM)
import           Control.Monad.Trans (lift)
import qualified Control.Concurrent.STM.TChan as TC
import           Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TQueue as TQ
import           Control.Concurrent.STM.TQueue (TQueue)
import qualified Data.IntMap as Map
import           Data.IntMap (IntMap)
import Debug.Trace (trace)


type Address = Int
type Memory = IntMap Int
type Program = [Int]

data Computer = Computer
  { name               :: String
  , memory             :: Memory
  , instructionPointer :: Address
  , input              :: STM Int
  , output             :: Int -> STM ()
  }

----------------------------------------------------------------------
-- OpCode and Instructions

-- | our computers command-set
data Instruction
  = Halt
  | Add Parameter Parameter Parameter
  | Mult Parameter Parameter Parameter
  | Input Parameter
  | Output Parameter
  | JumpIfTrue Parameter Parameter
  | JumpIfFalse Parameter Parameter
  | LessThan Parameter Parameter Parameter
  | Equals Parameter Parameter Parameter
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
instructionLength Halt = 1
instructionLength Add{} = 4
instructionLength Mult{} = 4
instructionLength Input{} = 2
instructionLength Output{} = 2
instructionLength JumpIfTrue{} = 3
instructionLength JumpIfFalse{} = 3
instructionLength LessThan{} = 4
instructionLength Equals{} = 4


----------------------------------------------------------------------
-- Interpretation of Programms

-- | using a stack of an state-monad containing the 'Program' and
--   Except String for error messages
--   over STM
type IntCodeM a = StateT Computer (ExceptT String IO) a


createComputer :: String -> Program -> (STM Int) -> (Int -> STM ()) -> Computer
createComputer n prg receive send = do
  Computer n (Map.fromList $ zip [0..] prg) 0 receive send


runComputer :: Computer -> IO (Either String ())
runComputer = Except.runExceptT . State.evalStateT executeProgram


-- | keeps running instructions until halt
executeProgram :: IntCodeM ()
executeProgram = do
  instr <- getCurrentInstruction
  case instr of
    Halt -> pure ()
    _ -> runInstruction instr >> executeProgram


-- | runs a single instruction according to the specifications of Day2 cc
runInstruction :: Instruction -> IntCodeM ()
runInstruction Halt = 
  -- no-op on halt
  pure $ seq (trace "HALT" ()) ()
runInstruction (Add locA locB (Parameter PositionMode locTgt)) = do
  -- add the value in from the first two given parameters and
  -- write the result to the third's address (only valid if it is in 'PositionMode')
  operandA <- evalParameter locA
  operandB <- evalParameter locB
  setMemory locTgt (operandA + operandB)
  moveNext
runInstruction (Mult locA locB (Parameter PositionMode locTgt)) = do
  -- multiplies the value in from the first two given parameters and
  -- write the result to the third's address (only valid if it is in 'PositionMode')
  operandA <- evalParameter locA
  operandB <- evalParameter locB
  setMemory locTgt (operandA * operandB)
  moveNext
runInstruction (JumpIfTrue cond loc) = do
  -- jumps to the address of the second parameter if the value of the first is
  -- non-zero - if not just moves to the next instruction
  condVal <- evalParameter cond
  jumpAdr <- evalParameter loc
  if condVal == 0 
    then moveNext
    else jumpTo jumpAdr
runInstruction (JumpIfFalse cond loc) = do
  -- jumps to the address of the second parameter if the value of the first is
  -- zero - if not just moves to the next instruction
  condVal <- evalParameter cond
  jumpAdr <- evalParameter loc
  if condVal == 0 
    then jumpTo jumpAdr
    else moveNext
runInstruction (LessThan a b (Parameter PositionMode locTgt)) = do
  -- compares parameter-values of the first and second parameter
  -- writes 1 to the address of the last parameter if the first value is less than the second
  -- if not writes 0
  aVal <- evalParameter a
  bVal <- evalParameter b
  let res = if aVal < bVal then 1 else 0
  setMemory locTgt res
  moveNext
runInstruction (Equals a b (Parameter PositionMode locTgt)) = do
  -- compares parameter-values of the first and second parameter
  -- writes 1 to the address of the last parameter if the first value is equal to the second
  -- if not writes 0
  aVal <- evalParameter a
  bVal <- evalParameter b
  let res = if aVal == bVal then 1 else 0
  setMemory locTgt res
  moveNext
runInstruction (Input (Parameter PositionMode toLoc)) = do
  -- reads the next input from the input "stream"
  -- and writes it to the address of the parameter
  inp <- readInput
  setMemory toLoc inp
  moveNext
runInstruction (Output from) = do
  -- gets the value from the parameter and writes it to the "output" stream
  out <- evalParameter from
  writeOutput out
  moveNext
runInstruction inst =
  -- all other cases should be an invalid instruction (wrong number or parameters for example)
  Except.throwError $ "invalid instruction: '" ++ show inst ++ "'"


-- | gets the instruction at the instruction-pointer
getCurrentInstruction :: IntCodeM Instruction
getCurrentInstruction = do
  adr <- getInstructionPointer
  getInstruction adr


-- | reads a instruction starting at the given memory address
getInstruction :: Address -> IntCodeM Instruction
getInstruction address = do
  (opCode, modes) <- parseInstrCode <$> getMemory address
  case opCode of
    99 -> pure Halt
    1  -> readInstruction' Add modes
    2  -> readInstruction' Mult modes
    3  -> readInstruction' Input modes
    4  -> readInstruction' Output modes
    5  -> readInstruction' JumpIfTrue modes
    6  -> readInstruction' JumpIfFalse modes
    7  -> readInstruction' LessThan modes
    8  -> readInstruction' Equals modes
    _ -> Except.throwError $ "unknown opcode " ++ show opCode
  where
    readInstruction' i modes = (\(_,_,x) -> x) <$> readInstruction i (address+1) modes
    parseInstrCode code =
      let opCode = code `mod` 100
          modes  = code `div` 100
      in (opCode, modes)
   
------------------------------------------------------------
-- small helper class to help the compiler figure out the
-- actual parameters-count while parsing for us

class ReadInstruction i where
  readInstruction :: i -> Address -> Int -> IntCodeM (Address, Int, Instruction)

instance ReadInstruction Instruction where
  readInstruction i adr modes = pure (adr, modes, i)

instance ReadInstruction i => ReadInstruction (Parameter -> i) where
  readInstruction p2i adr modes = do
    p <- readParam
    readInstruction (p2i p) (adr+1) (modes `div` 10)
    where
      readParam = do
        value <- getMemory adr
        mode <- parameterModeFromInt (modes `mod` 10)
        pure $ Parameter mode value

------------------------------------------------------------

-- | evaluates the parameter against the current memory content
--   if it's an 'ImmediateMode' parameter it's just it's value
--   if it's an 'PositionMode' parameter it's the memory content of the address
--   = the parameters value
evalParameter :: Parameter -> IntCodeM Int
evalParameter (Parameter ImmediateMode v) = pure v
evalParameter (Parameter PositionMode adr) = getMemory adr


-- | reads the memory value at the given address
getMemory :: Address -> IntCodeM Int
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
memoryBounds :: IntCodeM (Address, Address)
memoryBounds = do
  lower <- State.gets (fmap fst . Map.lookupMin . memory)
  upper <- State.gets (fmap fst . Map.lookupMax . memory)
  let bounds = (,) <$> lower <*> upper
  maybe (Except.throwError "memory is empty") pure bounds


-- | pops the next value from the input-stack and returns it
--   if no more values are on it fails with an error
readInput :: IntCodeM Int
readInput = do
  n <- State.gets name
  receive <- State.gets input
  lift $ lift $ STM.atomically receive


-- | pushes the value to the output-stack
writeOutput :: Int -> IntCodeM ()
writeOutput out = do
  n <- State.gets name
  send <- State.gets output
  lift $ lift $ STM.atomically $ send out


-- | gets the current instruction pointer
getInstructionPointer :: MonadState Computer m => m Address
getInstructionPointer = State.gets instructionPointer


-- | jumps to the given address by setting the instruction pointer to it
jumpTo :: Address -> IntCodeM ()
jumpTo = setInstructionPointer 


-- | moves the instruction-pointer to the next instruction after the current
moveNext :: IntCodeM ()
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