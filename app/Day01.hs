-- |
-- Module      :  Main
-- Description :  Day 1; Calorie Counting
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  2 11:19:05 2022.
--
-- See https://adventofcode.com/2022/day/1.
module Main
  ( main,
  )
where

import Control.Applicative (Alternative (some))
import Data.Attoparsec.Text.Lazy
  ( Parser,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
  )
import Data.List (sort)
import qualified Data.Text.Lazy.IO as TL

parseCalories :: Parser Int
parseCalories = sum <$> some (decimal <* endOfLine)

parseInput :: Parser [Int]
parseInput = parseCalories `sepBy1'` endOfLine <* endOfInput

main :: IO ()
main = do
  b <- TL.readFile "inputs/input01.txt"
  -- Part 1.
  let cs = either error id $ parseOnly parseInput b
  print $ maximum cs
  -- Part 2.
  let csSorted = reverse $ sort cs
  print $ sum $ take 3 csSorted
