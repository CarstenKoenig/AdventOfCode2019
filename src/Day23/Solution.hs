{-# LANGUAGE RecordWildCards #-}

module Day23.Solution where

import           Coords
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           IntCodePure


dayNr :: Int
dayNr = 23

type Input = Program Int

type NetAddress = Int

type Packet = Coord

data AdrPacket = AdrPacket
  { receiver   :: NetAddress
  , packet     :: Packet
  }

data Network = Network
  { nodes        :: M.Map NetAddress Node
  , network_nat  :: Maybe Packet
  , last_nat_out :: Maybe Packet
  }

data Node = Node
  { node_address    :: NetAddress
  , node_computer   :: Continuation Int
  , node_inputQueue :: [Int]
  , node_idle       :: Bool
  }


run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  prg <- loadInput

  let res1 = part1 prg
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 prg
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 1

part1 :: Input -> Int
part1 =
  snd . runNetwork . network


-- | run the network till one value is output for 255
runNetwork :: Network -> Packet
runNetwork net =
  let net' = foldl' stepNodeIn net [0..49]
  in case network_nat net' of
    Just res -> res
    Nothing  -> runNetwork net'


----------------------------------------------------------------------
-- Part 2

part2 :: Input -> Int
part2 = snd . runNetworkNAT . network


-- | run network till y-part of an NAT output to 0
--   is repeated twice in a row
runNetworkNAT :: Network -> Packet
runNetworkNAT net =
  let net' = foldl' stepNodeIn net [0..49]
  in case sendTo0IfIdle net' of
    Left res -> res
    Right net'' -> runNetworkNAT net''
  where
   sendTo0IfIdle net' =
     case network_nat net' of
       Nothing -> Right net'
       Just (nat@(x,y)) ->
         if allIdle net'
         then if fmap snd (last_nat_out net') == Just y
              then Left nat
              else Right $ net'
                   { nodes = M.adjust (\n -> n { node_inputQueue = node_inputQueue n ++ [x,y], node_idle = False }) 0 (nodes net')
                   , last_nat_out = Just nat
                   }
         else Right net'



----------------------------------------------------------------------
-- network functions


-- | creates the network
network :: Program Int -> Network
network prg = Network
  (M.fromList [ (adr, createNode prg adr) | adr <- [0..49] ])
  Nothing
  Nothing


-- | checks if all nodes in the network
--   are idle (no input queue and only asked for input in last Result)
allIdle :: Network -> Bool
allIdle Network{..} =
  all node_idle nodes


-- | runs a single node in the network to it's next Result
--   updating input the input queue if the receiver if the
--   node did output something
stepNodeIn :: Network -> NetAddress -> Network
stepNodeIn net@Network{..} adr =
  let node = nodes M.! adr
      (out, node') = stepNode node
  in setOutput out $ update node' nodes
  where
    update = M.insert adr
    setOutput Nothing nodes' = net { nodes = nodes'}
    setOutput (Just (AdrPacket recAdr p@(x,y))) nodes' =
      case recAdr of
        255 ->
          net { nodes = nodes', network_nat = Just p }
        _   ->
          net { nodes = M.adjust (\n' -> n' { node_inputQueue = node_inputQueue n' ++ [x,y], node_idle = False }) recAdr nodes' }


-- | runs one input node returning if neccessary
--   the output packet if there was any and the
--   new state of the node
stepNode :: Node -> (Maybe AdrPacket, Node)
stepNode n@Node{..} =
  case runComputer node_computer of
    Halted -> error $ "node " ++ show node_address ++ " halted"
    Error err -> error $ "node " ++ show node_address ++ " error: " ++ err
    NewOutput _ _ -> let Just (out, n') = readOutput n in (Just out, n' { node_idle = False })
    RequestInput i2c ->
      case node_inputQueue of
        [] -> (Nothing, n { node_computer = i2c (-1), node_idle = True })
        i:rest -> (Nothing, n { node_computer = i2c i, node_inputQueue = rest })


-- | sets up a node (creating a computer with single input of
--   the nodes address as asked in the problem)
createNode :: Program Int -> NetAddress -> Node
createNode prg adr =
  Node adr (initComputer prg) [adr] False


-- | tries to read 3 output values from a node
--   into a 'AdrPacket' value
readOutput :: Node -> Maybe (AdrPacket, Node)
readOutput n = do
  (recAdr, n') <- tryRead n
  (x, n'') <- tryRead n'
  (y, n''') <- tryRead n''
  pure (AdrPacket recAdr (x,y), n''')


-- | tries to read a single value from a node
tryRead :: Node -> Maybe (Int, Node)
tryRead n@Node{..} =
  case runComputer node_computer of
    NewOutput out cont -> Just (out, n { node_computer = cont })
    _                  -> Nothing


-- | loading and parsing of todays input-IntCode program
loadInput :: IO Input
loadInput =
  parseProgram <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )
