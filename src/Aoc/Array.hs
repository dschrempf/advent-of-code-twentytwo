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
    neighborsNoDiagonal,
    break,
  )
where

import Data.Massiv.Array
import Data.Maybe
import Prelude hiding (break)

stencil :: Sz Ix2 -> Ix2 -> [(Int, Int)]
stencil s p =
  [ p'
    | f <- [pred, id, succ],
      g <- [pred, id, succ],
      let p' = (f i, g j),
      isSafeIndex s (toIx2 p')
  ]
  where
    (i, j) = fromIx2 p

neighbors :: Sz Ix2 -> Ix2 -> [Ix2]
neighbors s p =
  [ toIx2 (i', j')
    | (i', j') <- stencil s p,
      not (i' == i && j' == j)
  ]
  where
    (i, j) = fromIx2 p

neighborsNoDiagonal :: Sz Ix2 -> Ix2 -> [Ix2]
neighborsNoDiagonal s p =
  [ toIx2 (i', j')
    | (i', j') <- stencil s p,
      not (i' == i && j' == j),
      i' == i || j' == j
  ]
  where
    (i, j) = fromIx2 p

break :: Manifest r e => (e -> Bool) -> Vector r e -> (Vector r e, Vector r e)
break p xs = sliceAt (Sz1 i) xs
  where
    i = fromMaybe 0 $ findIndex p xs
