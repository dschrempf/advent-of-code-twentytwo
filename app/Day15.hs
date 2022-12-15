{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 15; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/15.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

-- Position index (x, y).
type Ix = (Int, Int)

-- Size (from, to).
type Sz = (Int, Int)

type Sensors = M.Map Ix Int

type Beacons = S.Set Ix

manhattan :: Ix -> Ix -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Sensor with position and closest beacon.
data Sensor = Sensor Ix Ix
  deriving (Show, Eq)

pSensor :: Parser Sensor
pSensor = do
  _ <- string "Sensor at x="
  sx <- signed decimal
  _ <- string ", y="
  sy <- signed decimal
  _ <- string ": closest beacon is at x="
  bx <- signed decimal
  _ <- string ", y="
  by <- signed decimal
  pure $ Sensor (sx, sy) (bx, by)

pInput :: Parser [Sensor]
pInput = pSensor `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

hInput :: [Sensor] -> (Sensors, Beacons)
hInput = foldl' accFun (M.empty, S.empty)
  where
    accFun (ss, bs) (Sensor s b) =
      let d = manhattan s b
       in (M.insert s d ss, S.insert b bs)

size :: Sensors -> Sz
size ss = (mi, ma)
  where
    (mi, ma) = getMinMax $ unzip $ M.elems $ M.mapWithKey mm ss
    mm (x, _) d = (x - d, x + d)
    getMinMax (mins, maxs) = (minimum mins, maximum maxs)

-- Part 1.

canBeBeacon :: Sensors -> Beacons -> Ix -> Bool
canBeBeacon ss bs i
  | i `S.member` bs = True
  | otherwise = not anyInRange
  where
    inRangeP s d = manhattan s i <= d
    inRangeM = M.mapWithKey inRangeP ss
    anyInRange = or $ M.elems inRangeM

countNonBeacons :: Sz -> Sensors -> Beacons -> Int -> Int
countNonBeacons (mi, ma) ss bs r = length $ filter (== False) [canBeBeacon ss bs (x, r) | x <- [mi .. ma]]

-- Part 2.

fieldLen :: Int
-- fieldLen = 4000000
fieldLen = 20

-- tune :: Ix -> Int
-- tune (x, y) = x * 4000000 + y

-- frame :: Int -> [Ix]
-- frame n = [(x, f) | x <- [f .. t]] ++ [(x, t) | x <- [f .. t]]
--   where
--     f = n
--     t = fieldLen - n

-- findBeacon :: Sensors -> Beacons -> Maybe Ix
-- findBeacon ss bs =
--   listToMaybe
--     [ i
--       | i <- frame 0,
--         canBeBeacon ss bs i
--     ]

distressBeacon :: Sensors -> Beacons -> Ix -> Bool
distressBeacon ss bs i
  | i `S.member` bs = False
  | otherwise = not anyInRange
  where
    inRangeP s d = manhattan s i <= d
    inRangeM = M.mapWithKey inRangeP ss
    anyInRange = or $ M.elems inRangeM

findBeacon :: Sensors -> Beacons -> Maybe Ix
findBeacon ss bs =
  listToMaybe
    [ (x, y)
      | x <- [0 .. fieldLen],
        y <- [0 .. fieldLen],
        distressBeacon ss bs (x, y)
    ]

main :: IO ()
main = do
  d <- BS.readFile "inputs/input15-sample.txt"
  let xs = either error id $ parseOnly pInput d
      (ss, bs) = hInput xs
      sz = size ss
  -- -- Part 1.
  -- print sz
  -- print $ countNonBeacons sz ss bs 2000000
  -- Part 2.
  print sz
  print $ findBeacon ss bs
