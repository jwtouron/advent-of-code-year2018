module Day1 (solve1) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe

parseLine :: String -> Int
parseLine ('+':xs) = read xs
parseLine line = read line

getInput :: IO [Int]
getInput = map parseLine . lines <$> readFile "input/1.txt"

solve1a :: IO ()
solve1a = do
  lines <- getInput
  putStrLn $ show $ sum lines

data State =
  State { counts :: IntMap Int
        , freq :: Int
        , found :: Bool
        } deriving (Show)
newState = State { counts = IntMap.singleton 0 1, freq = 0, found = False }

solve1b_ :: [Int] -> Int
solve1b_ xs = freq $ head $ dropWhile (not . found) $ scanl update newState $ cycle xs
  where
    update State{counts=counts, freq=freq, found=found} x =
      let freq' = freq + x
          counts' = IntMap.insertWith (+) freq' 1 counts
          found' = counts' IntMap.! freq' == 2
      in State{counts = counts', freq = freq', found = found'}

solve1b :: IO ()
solve1b = do
  lines <- getInput
  putStrLn $ show $ solve1b_ lines

solve1 :: IO ()
solve1 = do
  solve1a
  solve1b
