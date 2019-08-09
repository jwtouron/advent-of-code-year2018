{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Day7 where

import           Control.Exception (assert)
import           Control.Monad.State
import           Data.Foldable (toList)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Text.Printf (printf)
import           Text.Regex.PCRE.Heavy (Regex, re, scan)

data Instruction =
  Instruction { instrReq :: Char
              , instrStep :: Char
              } deriving (Show)

instructionRegex :: Regex
instructionRegex = [re|Step ([A-Z]) must be finished before step ([A-Z]) can begin\.|]

parseInstruction :: Text -> Instruction
parseInstruction text =
  let (a:b:_) = (snd . head) $ scan instructionRegex text
  in Instruction (Text.head a) (Text.head b)

exampleInput :: [Instruction]
exampleInput = map parseInstruction text
  where
    text =
      [ "Step C must be finished before step A can begin."
      , "Step C must be finished before step F can begin."
      , "Step A must be finished before step B can begin."
      , "Step A must be finished before step D can begin."
      , "Step B must be finished before step E can begin."
      , "Step D must be finished before step E can begin."
      , "Step F must be finished before step E can begin."
      ]

input :: IO [Instruction]
input = map parseInstruction <$> Text.lines <$> Text.IO.readFile "input/7.txt"

data Worker =
  Worker { workerStep :: Char
         , workerTimeLeft :: Int
         } deriving (Show)

-- stepWorker :: Worker -> Worker
-- stepWorker worker@Worker{..} =
--   let workerTimeLeft' = if workerTimeLeft > 0 then workerTimeLeft - 1 else 0
--   in Worker { workerStep = if workerTimeLeft' == 0 then '.' else workerStep
--             , workerTimeLeft = workerTimeLeft'
--             }

isWorkerIdle :: Worker -> Bool
isWorkerIdle Worker{..} = workerStep == '.'

type Queue2 a = State ([a], [a])

queue2Empty = ([], [])

queue2Push :: a -> Queue2 a ()
queue2Push x = do
  modify' (\(ins, outs) -> (x:ins, outs))
  return ()

queue2Pop :: Queue2 a (Maybe a)
queue2Pop = do
  s <- get
  let (x, s') = f s
  put s'
  return x

  where
    f ([], []) = (Nothing, ([], []))
    f (is, []) = f ([], (reverse is))
    f (is, o:os) = (Just o, (is, os))

test = flip execState queue2Empty $ sequence [queue2Push 1, queue2Push 2]
test2 = flip runState test $ sequence [queue2Pop, queue2Pop]


data Queue a =
  Queue { queuePushList :: [a]
        , queuePopList :: [a]
        } deriving (Show)

mkQueue :: Queue a
mkQueue = Queue [] []

queuePush :: a -> Queue a -> Queue a
queuePush x (Queue pushList popList) = Queue (x:pushList) popList

queuePushAll :: [a] -> Queue a -> Queue a
queuePushAll xs q = foldr queuePush q xs

queuePop :: Queue a -> (Maybe a, Queue a)
queuePop q@(Queue [] []) = (Nothing, q)
queuePop (Queue pushList []) = queuePop $ Queue [] (reverse pushList)
queuePop (Queue pushList (x:popList)) = (Just x, Queue pushList popList)

queuePopN :: Queue a -> Int -> ([a], Queue a)
queuePopN q n = go n ([], q)
  where
    go 0 out = out
    go n (as, q) =
      case queuePop q of
        (Just a, q')  -> go (n - 1) ((a:as), q')
        (Nothing, q') -> (as, q')

data WorkerPool =
  WorkerPool { workerPoolWorkers :: [Worker]
             , workerPoolInQueue :: Queue Char
             , workerPoolOutQueue :: Queue Char
             , workerPoolInitTimeLeft :: Char -> Int
             }

instance Show WorkerPool where
  show WorkerPool{..} =
    printf "{ workerPoolWorkers = %s, workerPoolInQueue = %s, workerPoolOutQueue = %s }"
           (show workerPoolWorkers ) (show workerPoolInQueue) (show workerPoolOutQueue)

mkWorkerPool :: Int -> (Char -> Int) -> WorkerPool
mkWorkerPool n f = WorkerPool{..}
  where
    workerPoolWorkers = take n $ repeat Worker{workerStep = '.', workerTimeLeft = 0}
    workerPoolInQueue = mkQueue
    workerPoolOutQueue = mkQueue
    workerPoolInitTimeLeft = f

-- workerPoolNumIdle :: WorkerPool -> Int
-- workerPoolNumIdle WorkerPool{..} = length $ filter isWorkerFinished workerPoolWorkers

stepWorkerPool :: WorkerPool -> WorkerPool
stepWorkerPool wp@WorkerPool{..} = undefined
  where
    (workerPoolWorkers', outChars) = foldl' f ([], []) workerPoolWorkers
      where
        f (ws, cs) w@Worker{..} =
          if workerTimeLeft == 1
          then (Worker '.' 0:ws, workerStep:cs)
          else (w{workerTimeLeft = workerTimeLeft - 1}:ws, cs)
    numIdle = length $ filter isWorkerIdle workerPoolWorkers'
    (inChars, workerPoolInQueue') = queuePopN workerPoolInQueue numIdle
--    workerPoolWorkers'' = foldl' 

wp = mkWorkerPool 4 (const 2)


data StepState =
  StepState { stepStateCompleted :: [Char]
            , stepStateUnblocked :: [Char]
            , stepStateBlocked :: [(Char, Char)]
            , stepStateAllSteps :: Set Char
            , stepStateWorkerPool :: WorkerPool
            } deriving (Show)

mkStepState :: (Traversable a, Foldable a) => WorkerPool -> a Instruction -> StepState
mkStepState wp instrs = StepState completed unblocked blocked allSteps wp
  where
    completed = []
    unblocked = sort $ Set.toList $ foldl' (\s Instruction{..} -> Set.delete instrStep s) allSteps instrs
    blocked :: [(Char, Char)]
    blocked = toList $ fmap (\Instruction{..} -> (instrReq, instrStep)) instrs
    allSteps = foldl' f Set.empty instrs
      where
        f s Instruction{..} = Set.insert instrReq (Set.insert instrStep s)

step :: StepState -> StepState
step stepState@StepState{..} =
  stepState{ stepStateCompleted = stepStateCompleted'
           , stepStateUnblocked = stepStateUnblocked'
           , stepStateBlocked = stepStateBlocked'
           }
  where
    (headStep:tailSteps) = stepStateUnblocked
    stepStateCompleted' = stepStateCompleted ++ [headStep]
    stepStateUnblocked' = sort $ Set.toList $ foldl' f s stepStateBlocked'
      where
        s = Set.difference stepStateAllSteps (Set.fromList stepStateCompleted')
        f s (_, a) = Set.delete a s
    stepStateBlocked' = filter ((/= headStep) . fst) stepStateBlocked

isFullyStepped :: StepState -> Bool
isFullyStepped StepState{..} = length stepStateCompleted == Set.size stepStateAllSteps

stepFully :: StepState -> StepState
stepFully stepState
  | isFullyStepped stepState = stepState
  | otherwise                = stepFully $ step stepState

solve7a :: [Instruction] -> [Char]
solve7a = stepStateCompleted . stepFully . mkStepState (mkWorkerPool 1 (const 0))

solve7 :: IO ()
solve7 = do
  input <- input

  let sol = solve7a input
  assert (sol == "ACHOQRXSEKUGMYIWDZLNBFTJVP") $ putStrLn sol
