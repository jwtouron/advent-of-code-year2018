module Day2 (solve2) where

import Control.Exception (assert)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

input :: IO [String]
input = lines <$> readFile "input/2.txt"

contains2or3 :: String -> (Bool, Bool)
contains2or3 s = Map.foldl updateContains2Or3 (False, False) counts
  where
    counts = foldl' (\counts c -> Map.insertWith (+) c 1 counts) Map.empty s
    updateContains2Or3 cs@(c2, c3) c =
      case c of
        2 -> (True, c3)
        3 -> (c2, True)
        _ -> cs

solve2a :: [String] -> Int
solve2a input =
  let (count2, count3) = foldl' updateCounts (0, 0) $ map contains2or3 input
  in count2 * count3

  where
    updateCounts (count2, count3) cs =
      case cs of
        (True, True) -> (count2 + 1, count3 + 1)
        (True, False) -> (count2 + 1, count3)
        (False, True) -> (count2, count3 + 1)
        (False, False) -> (count2, count3)

differByOne :: String -> String -> Bool
differByOne s1 s2 = 1 == (foldl' (\x (c1, c2) -> if c1 /= c2 then x + 1 else x) 0 $ zip s1 s2)

solve2b :: [String] -> String
solve2b ss = head $ [ map fst $ filter (\(c1, c2) -> c1 == c2) $ zip s1 s2
                    | s1 <- ss
                    , s2 <- ss
                    , differByOne s1 s2]

solve2 :: IO ()
solve2 = do
  input <- input

  let solution2a = solve2a input
  putStrLn $ show $ assert (solution2a == 8715) solution2a

  let solution2b = solve2b input
  putStrLn $ show $ assert (solution2b == "fvstwblgqkhpuixdrnevmaycd") solution2b
