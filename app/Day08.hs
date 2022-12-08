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
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Unboxed as VU

type Field = MU.Matrix Int

pField :: String -> Field
pField = MU.fromLists . map (map (read . singleton)) . lines

-- Part 1.

isVisible :: Field -> (Int, Int) -> Int -> Bool
isVisible f (i, j) s =
  any
    v
    [ VU.take j theRow,
      VU.drop (j + 1) theRow,
      VU.take i theCol,
      VU.drop (i + 1) theCol
    ]
  where
    v = isVisibleOneDirection s
    theRow = MU.takeRow f i
    theCol = MU.takeColumn f j

isVisibleOneDirection :: Int -> VU.Vector Int -> Bool
isVisibleOneDirection s v
  | VU.null v = True
  | VU.any (>= s) v = False
  | otherwise = True

-- Part 2.

scenicScore :: Field -> (Int, Int) -> Int -> Int
scenicScore f (i, j) s =
  product $
    map
      v
      [ VU.reverse $ VU.take j theRow,
        VU.drop (j + 1) theRow,
        VU.reverse $ VU.take i theCol,
        VU.drop (i + 1) theCol
      ]
  where
    v = nVisible s
    theRow = MU.takeRow f i
    theCol = MU.takeColumn f j

nVisible :: Int -> VU.Vector Int -> Int
nVisible s xs = if VU.null rest then n else n + 1
  where
    (smaller, rest) = VU.break (>= s) xs
    n = VU.length smaller

main :: IO ()
main = do
  d <- readFile "inputs/input08.txt"
  let f = pField d
      v = MU.imap (isVisible f) f
  -- Part 1.
  print $ MU.foldl (+) 0 $ MU.map (\b -> if b then 1 else 0 :: Int) v
  -- Part 2.
  let n = MU.imap (scenicScore f) f
  print $ MU.foldl max 0 n
