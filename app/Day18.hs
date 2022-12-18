-- |
-- Module      :  Main
-- Description :  Day 18; Boiling Boulders
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/18.
module Main
  ( main,
  )
where

import Aoc.List
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

type Cube = (Int, Int, Int)

pCube :: Parser Cube
pCube = do
  i <- decimal
  _ <- char ','
  j <- decimal
  _ <- char ','
  k <- decimal
  pure (i, j, k)

pInput :: Parser [Cube]
pInput = pCube `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

diff :: Int -> Int -> Int
diff x y = abs $ x - y

neighbor :: Cube -> Cube -> Bool
neighbor (i1, j1, k1) (i2, j2, k2) = diff i1 i2 + diff j1 j2 + diff k1 k2 <= 1

nNeighbors :: [Cube] -> Int
nNeighbors = length . filter (True ==) . map (uncurry neighbor) . pairs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input18.txt"
  let cs = either error id $ parseOnly pInput d
      n = length cs
  print $ n * 6 - nNeighbors cs * 2
