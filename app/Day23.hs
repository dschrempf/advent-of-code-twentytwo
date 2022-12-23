-- |
-- Module      :  Main
-- Description :  Day 23; Unstable Diffusion
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/23.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Massiv.Array
  ( Array,
    B (..),
    Comp (..),
    D,
    Ix2 (..),
    Sz (..),
    (!>),
    (<!),
  )
import qualified Data.Massiv.Array as A

data Tile = Elf | Ground
  deriving (Show, Eq)

toChar :: Tile -> Char
toChar Elf = '#'
toChar Ground = '.'

pTile :: Parser Tile
pTile = Ground <$ char '.' <|> Elf <$ char '#'

isElf :: Tile -> Bool
isElf Elf = True
isElf Ground = False
{-# INLINE isElf #-}

isGround :: Tile -> Bool
isGround Elf = False
isGround Ground = True
{-# INLINE isGround #-}

type Field = Array B Ix2 Tile

type FieldD = Array D Ix2 Tile

pField :: Parser Field
pField = A.fromLists' Seq <$> some pTile `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

showField :: Field -> String
showField = unlines . map (map toChar) . A.toLists

-- Does the field need to be resized? Also useful to find the final, smallest
-- rectangle.
nRowsWithoutElf :: Field -> Int
nRowsWithoutElf xs = go 0
  where
    (Sz2 rows cols) = A.size xs
    rowsH = rows `div` 2
    colsH = cols `div` 2
    go n
      | n < rowsH && n < colsH =
          if any isElf nRow
            || any isElf sRow
            || any isElf wCol
            || any isElf eCol
            then n
            else go $ succ n
      | otherwise = pred n
      where
        nRow = xs !> 0
        sRow = xs !> pred rows
        wCol = xs <! 0
        eCol = xs <! pred cols

shrink :: Int -> Field -> Field
shrink n xs = A.compute $ A.extractFromTo' (Ix2 n n) (Ix2 (r - n) (c - n)) xs
  where
    (Sz2 r c) = A.size xs

enlarge :: Int -> Field -> Field
enlarge n xs = A.compute $ A.concat' 2 [rows, xsCols, rows]
  where
    (Sz2 r c) = A.size xs
    cols = A.replicate Seq (Sz2 r n) Ground
    rows = A.replicate Seq (Sz2 n (c + 2 * n)) Ground
    xsCols = A.computeAs B $ A.concat' 1 [cols, xs, cols]

-- Resize the field such that there is enough space for elves to move around.
resize :: Field -> Field
resize xs
  | n > 25 = shrink 15 xs
  | n < 3 = enlarge 10 xs
  where
    n = nRowsWithoutElf xs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input23-sample.txt"
  let f = either error id $ parseOnly pField d
  putStrLn $ showField $ resize f
  print $ nRowsWithoutElf f
