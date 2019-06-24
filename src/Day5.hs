{-# LANGUAGE ViewPatterns #-}

module Day5 (solve5) where

import           Control.Exception
import           Data.Char

input :: IO String
input = readFile "input/5.txt"

data Zipper =
  Zipper { prev :: [Char]
         , next :: [Char]
         } deriving Show

stringToZipper :: String -> Zipper
stringToZipper s = Zipper [] s

canStep :: Zipper -> Bool
canStep (Zipper _ []) = False
canStep (Zipper _ (x:[])) = False
canStep _ = True

willReact :: Char -> Char -> Bool
willReact c1 c2 = c1 /= c2 && toLower c1 == toLower c2

zipperToString :: Zipper -> String
zipperToString (Zipper prev next) = reverse prev ++ next

step :: Zipper -> Zipper
step zipper@(canStep -> False) = zipper
step (Zipper [] (x1:x2:xs)) =
  if willReact x1 x2
  then Zipper [] xs
  else Zipper [x1] (x2:xs)
step (Zipper (y:ys) (x1:x2:xs)) =
  if willReact x1 x2
  then Zipper ys (y:xs)
  else Zipper (x1:y:ys) (x2:xs)

reactFully :: String -> String
reactFully = loop . stringToZipper
  where
    loop zipper =
      if canStep zipper
      then loop $ step zipper
      else zipperToString zipper

solve5a :: String -> Int
solve5a = length . reactFully

solve5b :: String -> Int
solve5b s =
  minimum $ map (length . reactFully . filterChar s) ['a' .. 'z']
  where
    filterChar s c = filter (\c' -> c' /= c && c' /= toUpper c) s

solve5 :: IO ()
solve5 = do
  input >>= \i -> let sol = solve5a i in print $ assert (sol == 11668) sol
  input >>= \i -> let sol = solve5b i in print $ assert (sol == 4652) sol
