{-# LANGUAGE StrictData #-}

module Day6 (solve6) where

import           Control.Exception
import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read

data Point = Point { pointX :: Int, pointY :: Int } deriving (Eq,Ord,Show)
type N = Int
type S = Int
type E = Int
type W = Int
data Bounds = Bounds N S E W deriving (Show)

testInput :: [Point]
testInput =
  [ Point 1 1
  , Point 1 6
  , Point 8 3
  , Point 3 4
  , Point 5 5
  , Point 8 9
  ]

textToInt :: Text -> Int
textToInt = fst . fromRight undefined . Text.Read.decimal

input :: IO [Point]
input = map f . Text.lines <$> Text.IO.readFile "input/6.txt"
  where
    f line =
      let (x:y:[]) = Text.splitOn (Text.pack ", ") line
      in Point (textToInt x) (textToInt y)

gridBounds :: Traversable a => a Point -> Bounds
gridBounds = foldl' f (Bounds 999999 (-999999) 999999 (-999999))
  where
    f (Bounds n s e w) (Point x y) =
      let newN = if y < n then y else n
          newS = if y > s then y else s
          newE = if x < e then x else e
          newW = if x > w then x else w
      in Bounds newN newS newE newW

pointsInBounds :: Bounds -> [Point]
pointsInBounds (Bounds n s e w) = [Point x y | y <- [n .. s], x <- [e .. w]]

manhattenDistance :: Point -> Point -> Int
manhattenDistance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

sortCoords :: [Point] -> Point -> [Point]
sortCoords coords point = sortOn (\coord -> manhattenDistance coord point) coords

findInfinites :: Bounds -> [Point] -> Set Point
findInfinites (Bounds north south east west) coords = foldl' f Set.empty testPoints
  where
    testPoints = concat [ [Point x (north - dist) | x <- [west .. east]]
                        , [Point x (south + dist) | x <- [west .. east]]
                        , [Point (west - dist) y  | y <- [north .. south]]
                        , [Point (east + dist) y  | y <- [north .. south]]
                        ]
    dist = (max (south - north) (east - west)) * 2
    f s p = Set.insert (head $ sortCoords coords p) s

closestCoords :: [Point] -> Point -> [Point]
closestCoords coords point = take 2 $ sortCoords coords point

solve6a :: Bounds -> [Point] -> Int
solve6a bounds coords = 1 + largestArea
  where
    largestArea =
      snd
      $ maximumBy (comparing snd)
      $ Map.assocs
      $ foldl' f
               Map.empty
               (filter (\p -> not (Set.member p infinites || Set.member p nonInfinites)) (pointsInBounds bounds))
    f m p =
      let (p1:p2:_) = closestCoords coords p
      in case () of
        _ | manhattenDistance p p1 == manhattenDistance p p2 -> m
        _ | Set.member p1 infinites -> m
        _ -> Map.insertWith (+) p1 1 m
    infinites = findInfinites bounds coords
    nonInfinites = Set.difference (Set.fromList coords) infinites

solve6b :: Bounds -> [Point] -> Int -> Int
solve6b bounds@(Bounds n s e w) coords limit =
  length
  $ filter (\p -> totalPointDistance coords p < limit)
  $ pointsInBounds bigBounds
  where
    totalPointDistance coords p = foldl' (\n c -> n + manhattenDistance p c) 0 coords
    boundsHeight = s - n
    boundsWidth = e - w
    bigBounds = Bounds (n - boundsHeight) (s + boundsHeight) (e + boundsWidth) (w - boundsWidth)
    infinites = findInfinites bounds coords
    nonInfinites = Set.difference (Set.fromList coords) infinites

solve6 :: IO ()
solve6 = do
  input' <- input
  let bounds = gridBounds input'

  let sola = solve6a bounds input'
  print $ assert (sola == 4829) sola

  let solb = solve6b bounds input' 10000
  print $ assert (solb == 46966) solb
