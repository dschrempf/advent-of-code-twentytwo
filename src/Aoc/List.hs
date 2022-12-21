{-# LANGUAGE TupleSections #-}

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
    pairs,
    findCycle,
  )
where

-- | Chop up a list into chunks of a given length. O(n).
--
-- Copied from Agda.Utils.List.
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = ys : chop n zs
  where
    (ys, zs) = splitAt n xs

-- | Get all unordered pairs.
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

-- | Search for a cycle in a list.
findCycle ::
  -- | Maximum length.
  Int ->
  [Int] ->
  Maybe (Int, [Int])
findCycle maxLength = go 1
  where
    m = 4
    go :: Int -> [Int] -> Maybe (Int, [Int])
    go n xs
      | n <= maxLength =
          let h = take n xs
              xss = chop n $ take (m * n) xs
           in if all (== h) xss then Just (n, h) else go (n + 1) xs
      | otherwise = Nothing
