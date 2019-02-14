module Main where

import System.Environment
import Day1 (solve1)
import Day2 (solve2)
import Day3 (solve3)

main :: IO ()
main = do
  getArgs >>= mapM_ runPuzzle

  where
    runPuzzle puzzle =
      case puzzle of
        "day1" -> solve1
        "day2" -> solve2
        "day3" -> solve3
        _      -> putStrLn $ "Unknown puzzle: " ++ puzzle
