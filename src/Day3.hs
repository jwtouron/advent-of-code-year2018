{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day3 (solve3) where

import           Control.Exception
import           Data.Either
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Regex.TDFA (Regex, ExecOption)
import qualified Text.Regex.TDFA as Regex
import           Text.Regex.TDFA.String (compile, regexec)

lineRegex :: Regex
lineRegex = either error id $ compile Regex.defaultCompOpt (Regex.ExecOption True)
                                      "#([[:digit:]]+) @ ([[:digit:]]+),([[:digit:]]+): ([[:digit:]]+)x([[:digit:]]+)"

type ClaimID = Int
type SquareMap = Map (Int,Int) (Set ClaimID)

data Line =
  Line { claimID :: ClaimID
       , fromLeft :: Int
       , fromTop :: Int
       , width :: Int
       , height :: Int
       } deriving (Eq,Ord,Show)

parseLine :: String -> Line
parseLine line =
  let (_, _, _, [claimId, fromLeft, fromTop, width, height]) = either error fromJust $ regexec lineRegex line
  in Line (read claimId) (read fromLeft) (read fromTop) (read width) (read height)

input :: IO [Line]
input = map parseLine . lines <$> readFile "input/3.txt"

mkSquareMap :: [Line] -> SquareMap
mkSquareMap = foldl' updateMap Map.empty
  where
    updateMap map line@Line{ claimID, fromLeft, fromTop } =
      foldl' (\m s -> Map.insertWith Set.union s (Set.singleton claimID) m) map $ getLineSquares line

getLineSquares :: Line -> [(Int,Int)]
getLineSquares line@Line{ fromLeft, fromTop, height, width } =
  [ (x, y) | x <- [fromLeft .. fromLeft + width - 1], y <- [fromTop .. fromTop + height - 1] ]

solve3a :: SquareMap -> [Line] -> Int
solve3a squareMap lines = length $ filter ((1 <) . Set.size . snd) $ Map.toList squareMap

solve3b :: SquareMap -> [Line] -> Int
solve3b squareMap lines = head $ Set.elems $ Map.foldl updateClaimIDs claimIDs squareMap
  where
    claimIDs = Set.fromList $ map claimID lines
    updateClaimIDs s cs =
      if Set.size cs > 1 then
        Set.difference s cs
      else
        s

solve3 :: IO ()
solve3 = do
  squareMap <- mkSquareMap <$> input
  lines <- input

  let sol3a = solve3a squareMap lines
  putStrLn $ show $ assert (sol3a == 114946) sol3a

  let sol3b = solve3b squareMap lines
  putStrLn $ show $ assert (sol3b == 877) sol3b
