{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module IntCodeV1 
  ( Program, Computer
  , Address
  , parseProgram
  , eval
  , runComputer
  , getMemory
  , setMemory
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
  } deriving (Show)

type Parameter = Int

----------------------------------------------------------------------
-- OpCode and Instructions

data OpCode
  = Halt
  | Add
  | Mult
  deriving (Show)

data Instruction = Instruction
  { opCode     :: OpCode
  , parameters :: [Parameter]
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
eval :: Program -> StateT Computer (Except String) a -> Either String a
eval prg act = 
  let computer = Computer (Map.fromList $ zip [0..] prg) 0
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
runInstruction Instruction{ opCode = Add, parameters = [locA, locB, locTgt] } = do
  -- add the value in the addresses given by the first 2 parameters and
  -- write the result to the third
  operandA <- getMemory locA
  operandB <- getMemory locB
  setMemory locTgt (operandA + operandB)
  moveNext
runInstruction Instruction{ opCode = Mult, parameters = [locA, locB, locTgt] } = do
  -- multiply the value in the addresses given by the first 2 parameters and
  -- write the result to the third
  operandA <- getMemory locA
  operandB <- getMemory locB
  setMemory locTgt (operandA * operandB)
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
  opCode <- getMemory adr
  case opCode of
    99 -> pure $ Instruction Halt []
    1  -> Instruction Add <$> getParams 3 (adr+1)
    2  -> Instruction Mult <$> getParams 3 (adr+1)
    _ -> Except.throwError $ "unknown opcode " ++ show opCode
  where
    getParams 0 _ = pure []
    getParams n p = do
      param <- getMemory p
      rest <- getParams (n-1) (p+1)
      pure (param:rest)


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