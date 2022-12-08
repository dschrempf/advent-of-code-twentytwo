-- |
-- Module      :  Main
-- Description :  Day 8; Treetop Tree House
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Dec  8 21:39:46 2022.
--
-- See https://adventofcode.com/2022/day/8.
module Main
  ( main,
  )
where

import Data.List

type Field = [[Int]]

pField :: String -> Field
pField = map (map (read . singleton)) . lines

-- This was a nice idea, but it does not work because trees may be counted more times.
nVisible :: [Int] -> Int
nVisible = fst . foldl' f (0, 0)
  where
    f (n, maxSize) thisSize = if maxSize >= thisSize then (n, maxSize) else (n + 1, thisSize)

main :: IO ()
main = do
  d <- readFile "inputs/input08.txt"
  let fRows = pField d
      fCols = transpose fRows
      n = sum $ map (sum . map nVisible) [fRows, map reverse fRows, fCols, map reverse fCols]
  print n
