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

rowOfInterest :: Int
-- rowOfInterest = 10
rowOfInterest = 2000000

countNonBeacons :: Sz -> Sensors -> Beacons -> Int
countNonBeacons (mi, ma) ss bs =
  length $
    filter
      (== False)
      [canBeBeacon ss bs (x, rowOfInterest) | x <- [mi .. ma]]

-- Part 2.

fieldLen :: Int
-- fieldLen = 20
fieldLen = 4000000

tune :: Ix -> Int
tune (x, y) = x * 4000000 + y

rhombus :: Int -> [(Int, Int)]
rhombus d =
  (0, d)
    : (0, -d)
    : (d, 0)
    : (-d, 0)
    : xs
    ++ map (f2 negate) xs
    ++ map (fb negate) xs
    ++ map (f1 negate) xs
  where
    xs = zip [1 .. d - 1] $ reverse [1 .. d - 1]
    f1 f (x, y) = (f x, y)
    f2 f (x, y) = (x, f y)
    fb f (x, y) = (f x, f y)

perimeter :: Ix -> Int -> [Ix]
perimeter (x, y) d =
  [ (x + dx, y + dy)
    | (dx, dy) <- rhombus $ succ d
  ]

inRange :: Int -> Bool
inRange x = x >= 0 && x <= fieldLen

distressBeacon :: Sensors -> Ix -> Bool
distressBeacon ss i = not anyInRange
  where
    inRangeP s d = manhattan s i <= d
    inRangeM = M.mapWithKey inRangeP ss
    anyInRange = or $ M.elems inRangeM

findBeacon :: Sensors -> Maybe Ix
findBeacon ss =
  listToMaybe
    [ (x, y)
      | (s, d) <- M.toList ss,
        (x, y) <- perimeter s d,
        inRange x,
        inRange y,
        distressBeacon ss (x, y)
    ]

main :: IO ()
main = do
  d <- BS.readFile "inputs/input15.txt"
  let xs = either error id $ parseOnly pInput d
      (ss, bs) = hInput xs
      sz = size ss
  -- Part 1.
  print sz
  print $ countNonBeacons sz ss bs
  -- Part 2.
  let b = findBeacon ss
  print b
  print $ tune <$> b
