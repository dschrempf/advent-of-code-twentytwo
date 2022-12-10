-- |
-- Module      :  Aoc.List
-- Description :  Tools for lists
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sat Dec 10 15:15:27 2022.
module Aoc.List
  ( chop,
  )
where

-- | Chop up a list in chunks of a given length. O(n).
--
-- Copied from Agda.Utils.List.
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = ys : chop n zs
  where
    (ys, zs) = splitAt n xs
