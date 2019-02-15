{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4 (solve4) where

import           Control.Exception
import           Data.Either
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.IO as Text.IO
import           Data.Text.Read as Text.Read
import           Text.Regex.PCRE.Heavy (scan, re)

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int
type GuardID = Int

data DateTime =
  DateTime { year :: Year, month :: Month, day :: Day, hour :: Hour, minute :: Minute } deriving (Show,Eq,Ord)
data BeginsShift = BeginsShift DateTime GuardID deriving (Show)
newtype FallsAsleep =  FallsAsleep DateTime deriving (Show)
newtype WakesUp = WakesUp DateTime deriving (Show)

textToInt :: Text -> Int
textToInt = fst . fromRight undefined . Text.Read.decimal

parseDateTime :: Text -> (DateTime, Text)
parseDateTime input =
  let (_, year:month:day:hour:min:rest:_) =
        head $ scan [re|\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.*)|] input
  in (DateTime (textToInt year) (textToInt month) (textToInt day) (textToInt hour) (textToInt min), rest)

parseBeginsShift :: Text -> Maybe BeginsShift
parseBeginsShift input =
  let (dateTime, rest) = parseDateTime input
      scanResult = listToMaybe $ scan [re|Guard #(\d+) begins shift|] rest
  in fmap (\(_, guardID:_) -> BeginsShift dateTime (textToInt guardID)) scanResult

parseFallsAsleep :: Text -> Maybe FallsAsleep
parseFallsAsleep input =
  let (dateTime, rest) = parseDateTime input
      scanResult = listToMaybe $ scan [re|falls asleep|] rest
  in fmap (const (FallsAsleep dateTime)) scanResult

parseWakesUp :: Text -> Maybe WakesUp
parseWakesUp input =
  let (dateTime, rest) = parseDateTime input
      scanResult = listToMaybe $ scan [re|wakes up|] rest
  in fmap (const (WakesUp dateTime)) scanResult

data Record = RecordBeginsShift BeginsShift
            | RecordFallsAsleep FallsAsleep
            | RecordWakesUp WakesUp
            deriving (Show)

recordDateTime :: Record -> DateTime
recordDateTime (RecordBeginsShift (BeginsShift dateTime _)) = dateTime
recordDateTime (RecordWakesUp (WakesUp dateTime)) = dateTime
recordDateTime (RecordFallsAsleep (FallsAsleep dateTime)) = dateTime

parseRecord :: Text -> Record
parseRecord input =
  head $ catMaybes [ RecordBeginsShift <$> parseBeginsShift input
                   , RecordFallsAsleep <$> parseFallsAsleep input
                   , RecordWakesUp <$> parseWakesUp input]

parseRecords :: Text -> [Record]
parseRecords = map parseRecord . Text.lines

newtype SortedRecords = SortedRecords { unSortedRecords :: [Record] }

input :: IO SortedRecords
input =
  SortedRecords
  . sortOn recordDateTime
  . parseRecords <$> Text.IO.readFile "input/4.txt"

type GuardSleepMinutes = IntMap (IntMap Int)

countGuardSleepMinutes :: SortedRecords -> GuardSleepMinutes
countGuardSleepMinutes (SortedRecords records) = counts
  where
    (_, _, counts) = foldl' f (-1, -1, IntMap.empty) records
    f (guard, sleepTime, counts) (RecordBeginsShift (BeginsShift (DateTime _ _ _ _ minute) newGuard)) =
      (newGuard, -1, updateCounts counts guard sleepTime minute)
    f (guard, sleepTime, counts) (RecordFallsAsleep (FallsAsleep (DateTime _ _ _ _ minute))) =
      (guard, minute, counts)
    f (guard, sleepTime, counts) (RecordWakesUp (WakesUp (DateTime _ _ _ _ minute))) =
      (guard, -1, updateCounts counts guard sleepTime minute)
    updateCounts counts guard sleepTime wakeupTime
      | guard == -1 || sleepTime == -1 = counts
      | otherwise =
        foldl' (\cs m -> IntMap.insertWith (IntMap.unionWith (+)) guard (IntMap.singleton m 1) cs)
               counts
               [sleepTime .. wakeupTime - 1]

solve4a :: GuardSleepMinutes -> Int
solve4a guardSleepMinutes = sleepiestGuard * mostCommonMinute
  where
    mostCommonMinute =
      fst
      $ maximumBy (comparing snd)
      $ IntMap.assocs
      $ guardSleepMinutes IntMap.! sleepiestGuard
    sleepiestGuard =
      fst
      $ maximumBy (comparing snd)
      $ IntMap.assocs
      $ IntMap.foldrWithKey f IntMap.empty guardSleepMinutes
    f g m2 m1 =
      let totalMinutes = IntMap.foldr (+) 0 m2
      in IntMap.insert g totalMinutes m1

solve4b :: GuardSleepMinutes -> Int
solve4b guardSleepMinutes = sleepiestGuard * sleepiestMinute
  where
    sleepiestMinute =
      fst
      $ maximumBy (comparing snd)
      $ IntMap.assocs
      $ guardSleepMinutes IntMap.! sleepiestGuard
    sleepiestGuard =
      fst
      $ maximumBy (comparing snd)
      $ IntMap.assocs
      $ IntMap.foldrWithKey f IntMap.empty guardSleepMinutes
    f g m2 m1 =
      let sleepiestMinute = snd $ maximumBy (comparing snd) $ IntMap.assocs m2
      in IntMap.insert g sleepiestMinute m1

solve4 :: IO ()
solve4 = do
  input' <- input
  let guardSleepMinutes = countGuardSleepMinutes input'

  let sol4a = solve4a guardSleepMinutes
  print $ assert (sol4a == 8421) sol4a

  let sol4b = solve4b guardSleepMinutes
  print $ assert (sol4b == 83359) sol4b

testInput :: SortedRecords
testInput = SortedRecords $ sortOn recordDateTime $ parseRecords text
  where
    text =
      "[1518-11-01 00:00] Guard #10 begins shift\n\
      \[1518-11-01 00:05] falls asleep\n\
      \[1518-11-01 00:25] wakes up\n\
      \[1518-11-01 00:30] falls asleep\n\
      \[1518-11-01 00:55] wakes up\n\
      \[1518-11-01 23:58] Guard #99 begins shift\n\
      \[1518-11-02 00:40] falls asleep\n\
      \[1518-11-02 00:50] wakes up\n\
      \[1518-11-03 00:05] Guard #10 begins shift\n\
      \[1518-11-03 00:24] falls asleep\n\
      \[1518-11-03 00:29] wakes up\n\
      \[1518-11-04 00:02] Guard #99 begins shift\n\
      \[1518-11-04 00:36] falls asleep\n\
      \[1518-11-04 00:46] wakes up\n\
      \[1518-11-05 00:03] Guard #99 begins shift\n\
      \[1518-11-05 00:45] falls asleep\n\
      \[1518-11-05 00:55] wakes up"
