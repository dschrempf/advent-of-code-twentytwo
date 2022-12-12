{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Aoc.Array
-- Description :  Tools for arrays
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Dec 12 11:41:00 2022.
module Aoc.Array
  ( neighbors,
  )
where

import Data.Massiv.Array

neighbors :: Sz Ix2 -> Ix2 -> [Ix2]
neighbors s p =
  [ p'
    | f <- [pred, id, succ],
      let i' = f i,
      g <- [pred, id, succ],
      let j' = g j,
      not (i' == i && j' == j),
      let p' = toIx2 (i', j'),
      isSafeIndex s p'
  ]
  where
    (i, j) = fromIx2 p
