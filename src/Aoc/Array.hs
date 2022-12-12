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
  ( Index (isSafeIndex),
    Ix2,
    Manifest,
    Size (size),
    Sz (Sz),
    Vector,
    findIndex,
    fromIx2,
    sliceAt,
    toIx2,
  )
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

-- | Get the 8 neighbors of a field in a two-dimensional grid.
neighbors :: Sz Ix2 -> Ix2 -> [Ix2]
neighbors s p =
  [ toIx2 (i', j')
    | (i', j') <- stencil s p,
      not (i' == i && j' == j)
  ]
  where
    (i, j) = fromIx2 p

-- | Like 'neighbors' but only get the 4 direct neighbors, and not the 4
-- diagonal ones.
neighborsNoDiagonal :: Sz Ix2 -> Ix2 -> [Ix2]
neighborsNoDiagonal s p =
  [ toIx2 (i', j')
    | (i', j') <- stencil s p,
      not (i' == i && j' == j),
      i' == i || j' == j
  ]
  where
    (i, j) = fromIx2 p

-- | Like 'Data.List.break' but for arrays.
break :: Manifest r e => (e -> Bool) -> Vector r e -> (Vector r e, Vector r e)
break p xs = sliceAt i xs
  where
    i = maybe (size xs) Sz $ findIndex p xs
