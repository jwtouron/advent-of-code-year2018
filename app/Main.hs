module Main where

import System.Environment
import Day1 (solve1)
import Day2 (solve2)
import Day3 (solve3)
import Day4 (solve4)
import Day5 (solve5)
import Day6 (solve6)

main :: IO ()
main = do
  getArgs >>= mapM_ runPuzzle

  where
    runPuzzle puzzle =
      case puzzle of
        "day1" -> solve1
        "day2" -> solve2
        "day3" -> solve3
        "day4" -> solve4
        "day5" -> solve5
        "day6" -> solve6
        _      -> putStrLn $ "Unknown puzzle: " ++ puzzle
