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
  ( nTimesStrict,
    nTimesLazy,
  )
where

import Control.DeepSeq

-- Apply a function @n@ times.
nTimesStrict :: NFData a => Int -> (a -> a) -> a -> a
nTimesStrict n f x = case compare n 1 of
  LT -> error $ "nTimesStrict: n zero or negative: " ++ show n
  EQ -> force $ f x
  GT -> nTimesStrict (n - 1) f $ force $ f x

nTimesLazy :: Int -> (a -> a) -> a -> a
nTimesLazy n f x = case compare n 1 of
  LT -> error $ "nTimesLazy: n zero or negative: " ++ show n
  EQ -> f x
  GT -> nTimesLazy (n - 1) f $ f x
