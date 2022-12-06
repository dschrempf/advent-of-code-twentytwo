-- |
-- Module      :  Main
-- Description :  Day 4; Camp Cleanup
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec  4 09:55:52 2022.
--
-- See https://adventofcode.com/2022/day/4.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Numeric.Natural

data Assignment = Assignment Natural Natural
  deriving (Show)

pAssignment :: Parser Assignment
pAssignment = do
  x <- decimal
  _ <- char '-'
  y <- decimal
  pure $ Assignment x y

pPair :: Parser (Assignment, Assignment)
pPair = do
  x <- pAssignment
  _ <- char ','
  y <- pAssignment
  pure (x, y)

pInput :: Parser [(Assignment, Assignment)]
pInput = pPair `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

contains :: Assignment -> Assignment -> Bool
contains (Assignment a b) (Assignment c d) = a <= c && b >= d

contain :: Assignment -> Assignment -> Bool
contain x y = contains x y || contains y x

overlaps :: Assignment -> Assignment -> Bool
overlaps (Assignment a b) (Assignment c d)
  | a <= c = b >= c
  | b >= d = a <= d
  | otherwise = False

overlap :: Assignment -> Assignment -> Bool
overlap x y = overlaps x y || overlaps y x

main :: IO ()
main = do
  b <- BS.readFile "inputs/input04.txt"
  let ps = either error id $ parseOnly pInput b
  -- Part 1.
  print $ length $ filter (== True) $ map (uncurry contain) ps
  -- Part 2.
  print $ length $ filter (== True) $ map (uncurry overlap) ps
