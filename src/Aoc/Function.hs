-- |
-- Module      :  Aoc.Function
-- Description :  Tools for functions
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 08:27:51 2022.
module Aoc.Function
  ( nTimes,
  )
where

import Control.DeepSeq

-- Apply a function @n@ times.
nTimes :: NFData a => Int -> (a -> a) -> a -> a
nTimes n f x = case compare n 1 of
  LT -> error $ "nTimes: n zero or negative: " ++ show n
  EQ -> force $ f x
  GT -> nTimes (n - 1) f $ force $ f x
