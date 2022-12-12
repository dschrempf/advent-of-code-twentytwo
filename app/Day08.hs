{-# LANGUAGE FlexibleContexts #-}

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

import qualified Aoc.Array as A
import Data.List (singleton)
import qualified Data.Massiv.Array as A
import Data.Semigroup (Max (Max), Sum)

type Field = A.Array A.U A.Ix2 Int

pField :: String -> Field
pField = A.fromLists' A.Seq . map (map (read . Data.List.singleton)) . lines

-- Part 1.

isVisible :: Field -> A.Ix2 -> Int -> Bool
isVisible f (A.Ix2 i j) s =
  any
    v
    [ A.take (A.Sz1 j) thisRow,
      A.drop (A.Sz1 $ succ j) thisRow,
      A.take (A.Sz1 i) thisCol,
      A.drop (A.Sz1 $ succ i) thisCol
    ]
  where
    v = isVisibleOneDirection s . A.compute
    thisRow = A.delay $ f A.!> i
    thisCol = f A.<! j

isVisibleOneDirection :: Int -> A.Vector A.U Int -> Bool
isVisibleOneDirection s v
  | A.any (>= s) v = False
  | otherwise = True

-- Part 2.

scenicScore :: Field -> A.Ix2 -> Int -> Int
scenicScore f (A.Ix2 i j) s =
  product $
    map
      v
      [ A.reverse A.Dim1 $ A.take (A.Sz1 j) thisRow,
        A.drop (A.Sz1 $ succ j) thisRow,
        A.reverse A.Dim1 $ A.take (A.Sz1 i) thisCol,
        A.drop (A.Sz1 $ succ i) thisCol
      ]
  where
    v = nVisible s . A.compute
    thisRow = A.delay $ f A.!> i
    thisCol = f A.<! j

nVisible :: Int -> A.Vector A.U Int -> Int
nVisible s xs = if A.isNull rest then n else n + 1
  where
    (smaller, rest) = A.break (>= s) xs
    n = A.unSz $ A.size smaller

main :: IO ()
main = do
  d <- readFile "inputs/input08.txt"
  let f = pField d
      v = A.imap (isVisible f) f
  -- Part 1.
  print $ A.fold $ A.map (\b -> if b then 1 else 0 :: Sum Int) v
  -- Part 2.
  let n = A.imap (scenicScore f) f
  print $ A.fold $ A.map Max n
