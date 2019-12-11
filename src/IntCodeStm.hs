{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

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
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import Debug.Trace (trace, traceM)


type Address adr = adr
type Memory n = Map n n
type Program n = [n]

data Computer n = Computer
  { name               :: String
  , memory             :: Memory n
  , instructionPointer :: Address n
  , relativeBase       :: Address n
  , input              :: STM n
  , output             :: n -> STM ()
  }

----------------------------------------------------------------------
-- OpCode and Instructions

-- | our computers command-set
data Instruction n
  = Halt
  | Add (Parameter n) (Parameter n) (Parameter n)
  | Mult (Parameter n) (Parameter n) (Parameter n)
  | Input (Parameter n)
  | Output (Parameter n)
  | JumpIfTrue (Parameter n) (Parameter n)
  | JumpIfFalse (Parameter n) (Parameter n)
  | LessThan (Parameter n) (Parameter n) (Parameter n)
  | Equals (Parameter n) (Parameter n) (Parameter n)
  | AdjustRelativeBase (Parameter n)
  deriving (Show)


-- | a parameter for an instruction
--   each parameter has a mode and a value
--   the mode flags if the value is to be interpreted as an address or as an value
data Parameter n = Parameter
  { parameterMode  :: ParameterMode
  , parameterValue :: n
  } deriving (Show)


-- | the position mode determins if a parameter is
--   interpreted as a pointer to a location ('PositionMode')
--   or as an actual constant value ('ImmediateMode')
data ParameterMode
  = PositionMode
  | ImmediateMode
  | RelativeMode
  deriving (Show, Eq)


-- | parses the parameter mode from the numbers given in the problem/description
parameterModeFromInt :: (Num n, Eq n, Show n) => MonadError String m => n -> m ParameterMode
parameterModeFromInt 0 = pure PositionMode
parameterModeFromInt 1 = pure ImmediateMode
parameterModeFromInt 2 = pure RelativeMode
parameterModeFromInt m = Except.throwError $ "invalid parameter-mode " ++ show m


-- | calculates the length of an instruction
--   so that we can move the instruction-pointer to the next one
instructionLength :: Num n => Instruction n -> n
instructionLength Halt = 1
instructionLength Add{} = 4
instructionLength Mult{} = 4
instructionLength Input{} = 2
instructionLength Output{} = 2
instructionLength JumpIfTrue{} = 3
instructionLength JumpIfFalse{} = 3
instructionLength LessThan{} = 4
instructionLength Equals{} = 4
instructionLength AdjustRelativeBase{} = 2


----------------------------------------------------------------------
-- Interpretation of Programms

-- | using a stack of an state-monad containing the 'Program' and
--   Except String for error messages
--   over STM
type IntCodeM n a = StateT (Computer n) (ExceptT String IO) a


createComputer 
  :: (Integral n, Show n, Eq n) 
  => String -> Program n -> STM n -> (n -> STM ()) -> Computer n
createComputer n prg =
  Computer n (Map.fromList $ zip [0..] prg) 0 0


runComputer :: (Eq n, Integral n, Show n) => Computer n -> IO (Either String ())
runComputer = Except.runExceptT . State.evalStateT executeProgram


-- | keeps running instructions until halt
executeProgram :: (Eq n, Integral n, Show n) => IntCodeM n ()
executeProgram = do
  instr <- getCurrentInstruction
  case instr of
    Halt -> pure ()
    _ -> runInstruction instr >> executeProgram


-- | runs a single instruction according to the specifications of Day2 cc
runInstruction :: (Eq n, Integral n, Show n) => Instruction n -> IntCodeM n ()
runInstruction Halt =
  -- no-op on halt
  pure $ seq (trace "HALT" ()) ()
runInstruction (Add locA locB tgt) = do
  -- add the value in from the first two given parameters and
  -- write the result to the third's address (only valid if it is in 'PositionMode')
  summandA <- evalParameter locA
  summandB <- evalParameter locB
  locTgt <- evalAddress tgt
  setMemory locTgt (summandA + summandB)
  moveNext
runInstruction (Mult locA locB tgt) = do
  -- multiplies the value in from the first two given parameters and
  -- write the result to the third's address (only valid if it is in 'PositionMode')
  operandA <- evalParameter locA
  operandB <- evalParameter locB
  locTgt <- evalAddress tgt
  setMemory locTgt (operandA * operandB)
  moveNext
runInstruction (JumpIfTrue cond loc) = do
  -- jumps to the address of the second parameter if the value of the first is
  -- non-zero - if not just moves to the next instruction
  condVal <- evalParameter cond
  jumpAdr <- evalParameter loc
  if condVal == 0
    then moveNext
    else jumpTo (fromIntegral jumpAdr)
runInstruction (JumpIfFalse cond loc) = do
  -- jumps to the address of the second parameter if the value of the first is
  -- zero - if not just moves to the next instruction
  condVal <- evalParameter cond
  jumpAdr <- evalParameter loc
  if condVal == 0
    then jumpTo (fromIntegral jumpAdr)
    else moveNext
runInstruction (LessThan a b tgt) = do
  -- compares parameter-values of the first and second parameter
  -- writes 1 to the address of the last parameter if the first value is less than the second
  -- if not writes 0
  locTgt <- evalAddress tgt
  aVal <- evalParameter a
  bVal <- evalParameter b
  let res = if aVal < bVal then 1 else 0
  setMemory locTgt res
  moveNext
runInstruction (Equals a b tgt) = do
  -- compares parameter-values of the first and second parameter
  -- writes 1 to the address of the last parameter if the first value is equal to the second
  -- if not writes 0
  aVal <- evalParameter a
  bVal <- evalParameter b
  locTgt <- evalAddress tgt
  let res = if aVal == bVal then 1 else 0
  setMemory locTgt res
  moveNext
runInstruction (Input tgt) = do
  -- reads the next input from the input "stream"
  -- and writes it to the address of the parameter
  inp <- readInput
  locTgt <- evalAddress tgt
  setMemory locTgt inp
  moveNext
runInstruction (Output from) = do
  -- gets the value from the parameter and writes it to the "output" stream
  out <- evalParameter from
  writeOutput out
  moveNext
runInstruction (AdjustRelativeBase deltaP) = do
  -- adjust the relative-base value
  delta <- evalParameter deltaP
  moveRelativeBase delta
  moveNext


-- | gets the instruction at the instruction-pointer
getCurrentInstruction :: (Integral n, Show n) => IntCodeM n (Instruction n)
getCurrentInstruction = do
  adr <- getInstructionPointer
  getInstruction adr


-- | reads a instruction starting at the given memory address
getInstruction :: forall n. (Integral n, Show n) => Address n -> IntCodeM n (Instruction n)
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
    9  -> readInstruction' AdjustRelativeBase modes
    _ -> Except.throwError $ "unknown opcode " ++ show opCode
  where
    readInstruction' i modes = (\(_,_,x) -> x) <$> readInstruction i (address+1) modes
    parseInstrCode :: n -> (n,n)
    parseInstrCode code =
      let opCode = code `mod` 100
          modes  = code `div` 100
      in (opCode, modes)

------------------------------------------------------------
-- small helper class to help the compiler figure out the
-- actual parameters-count while parsing for us

class ReadInstruction n i | i -> n where
  readInstruction :: i -> Address n -> n -> IntCodeM n (Address n, n, Instruction n)

instance ReadInstruction n (Instruction n) where
  readInstruction i adr modes = pure (adr, modes, i)

instance (Integral n, Show n, Eq n, ReadInstruction n i) => ReadInstruction n (Parameter n -> i) where
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
evalParameter :: (Integral n, Show n) => Parameter n -> IntCodeM n n
evalParameter (Parameter ImmediateMode v) = pure v
evalParameter (Parameter PositionMode adr) = getMemory (fromIntegral adr)
evalParameter (Parameter RelativeMode rel) = do
  base <- getRelativeBase
  getMemory (base + fromIntegral rel)

-- | as of part 7 RelativeMode is allowed for output parameters too
--   so this function takes care of adjusting the address to the relative
--   base in this case
evalAddress :: (Integral n, Show n) => Parameter n -> IntCodeM n (Address n)
evalAddress (Parameter ImmediateMode _) =
  Except.throwError "Immediate-Mode in Addressing"
evalAddress (Parameter PositionMode adr) = pure adr
evalAddress (Parameter RelativeMode rel) = do
  base <- getRelativeBase
  pure $ base + rel


-- | reads the memory value at the given address
getMemory :: (Ord n, Num n, Show n) => Address n -> IntCodeM n n
getMemory adr | adr < 0 =
  Except.throwError $ "tried to access the negative memory address " ++ show adr
getMemory adr = do
  found <- State.gets (Map.lookup adr . memory)
  case found of
    Just value -> pure value
    Nothing    -> pure 0


-- | sets the memory at the given address to a new value
setMemory :: Ord n => MonadState (Computer n) m => Address n -> n -> m ()
setMemory adr val =
  State.modify (\prg -> prg { memory = Map.insert adr val (memory prg) })


-- | get's the lowest and higest valid address in memory
memoryBounds :: IntCodeM n (Address n, Address n)
memoryBounds = do
  lower <- State.gets (fmap fst . Map.lookupMin . memory)
  upper <- State.gets (fmap fst . Map.lookupMax . memory)
  let bounds = (,) <$> lower <*> upper
  maybe (Except.throwError "memory is empty") pure bounds


-- | pops the next value from the input-stack and returns it
--   if no more values are on it fails with an error
readInput :: Show n => IntCodeM n n
readInput = do
  receive <- State.gets input
  lift $ lift $ STM.atomically receive


-- | pushes the value to the output-stack
writeOutput :: Show n => n -> IntCodeM n ()
writeOutput out = do
  send <- State.gets output
  lift $ lift $ STM.atomically $ send out


-- | gets the current instruction pointer
getInstructionPointer :: MonadState (Computer n) m => m (Address n)
getInstructionPointer = State.gets instructionPointer


-- | jumps to the given address by setting the instruction pointer to it
jumpTo :: Address n -> IntCodeM n ()
jumpTo = setInstructionPointer


-- | moves the instruction-pointer to the next instruction after the current
moveNext :: (Show n, Eq n, Integral n) => IntCodeM n ()
moveNext = do
  inst <- getCurrentInstruction
  moveInstructionPointer (instructionLength inst)


-- | moves the instruction-pointer to the current address increased by the given value
moveInstructionPointer :: Num n => MonadState (Computer n) m => Address n -> m ()
moveInstructionPointer delta = do
  ip <- getInstructionPointer
  setInstructionPointer (ip + delta)


-- | sets the instruction pointer to the new value
setInstructionPointer :: MonadState (Computer n) m => Address n -> m ()
setInstructionPointer ip =
  State.modify (\prg -> prg { instructionPointer = ip })


-- | gets the current relative-base
getRelativeBase :: MonadState (Computer n) m => m (Address n)
getRelativeBase =
  State.gets relativeBase


-- | adjusts the current relative-base by adding the 'delta' paramter
moveRelativeBase :: Num n => MonadState (Computer n) m => Address n -> m ()
moveRelativeBase delta =
  State.modify (\prg -> prg { relativeBase = relativeBase prg + delta })


-- | parses a program from a string
--   assumes the input are just integers separated by ',' characters and
--   possible whitespace
--   tries doing so by rewriting the input as a Haskell list and 'read' this
parseProgram :: (Read n) => String -> Program n
parseProgram input =
  read $ "[" ++ input ++ "]"