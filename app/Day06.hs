-- |
-- Module      :  Main
-- Description :  Day 6; Tuning Trouble
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Dec  5 23:57:50 2022.
--
-- See https://adventofcode.com/2022/day/6.
module Main
  ( main,
  )
where

import Data.Int
import Data.List
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

-- Part 1.

containsDouble :: Int64 -> TL.Text -> Bool
containsDouble n = (== n) . genericLength . nub . TL.unpack

getPosFirstData :: Int64 -> TL.Text -> Int64
getPosFirstData n x = go n (TL.take n x) (TL.drop n x)
  where
    go :: Int64 -> TL.Text -> TL.Text -> Int64
    go p w t = if containsDouble n w then p else go (p + 1) (TL.tail w `TL.snoc` TL.head t) $ TL.tail t

-- Nothing -> error "getPosFirstData: go: reached end of input"
-- (Just (x, xs)) -> undefined

main :: IO ()
main = do
  d <- TL.strip <$> TL.readFile "inputs/input06.txt"
  -- Part 1.
  print $ getPosFirstData 4 d
  -- Part 2.
  print $ getPosFirstData 14 d
