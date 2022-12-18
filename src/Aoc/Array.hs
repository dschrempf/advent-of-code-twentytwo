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
    neighbors3,
    neighborsNoDiagonal3,
    break,
  )
where

import Data.Massiv.Array
  ( Index (isSafeIndex),
    Ix2,
    Ix3,
    IxN (Ix3),
    Manifest,
    Size (size),
    Sz (Sz),
    Vector,
    findIndex,
    fromIx2,
    fromIx3,
    sliceAt,
    toIx2,
    toIx3,
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

stencil3 :: Sz Ix3 -> Ix3 -> [(Int, Int, Int)]
stencil3 s p =
  [ p'
    | f <- [pred, id, succ],
      g <- [pred, id, succ],
      h <- [pred, id, succ],
      let p' = (f i, g j, h k),
      isSafeIndex s (toIx3 p')
  ]
  where
    (i, j, k) = fromIx3 p

-- | Get the 26 neighbors of a field in a three-dimensional grid.
neighbors3 :: Sz Ix3 -> Ix3 -> [Ix3]
neighbors3 s p =
  [ toIx3 (i', j', k')
    | (i', j', k') <- stencil3 s p,
      not (i' == i && j' == j && k' == k)
  ]
  where
    (i, j, k) = fromIx3 p

-- | Like 'neighbors3' but only get the 6 direct neighbors, and not the 20
-- diagonal ones.
neighborsNoDiagonal3 :: Sz Ix3 -> Ix3 -> [Ix3]
neighborsNoDiagonal3 s p =
  [ f p di
    | di <- [1, -1],
      f <- fs,
      let p' = f p di,
      isSafeIndex s p'
  ]
  where
    fs =
      [ \(Ix3 x y z) dx -> Ix3 (x + dx) y z,
        \(Ix3 x y z) dy -> Ix3 x (y + dy) z,
        \(Ix3 x y z) dz -> Ix3 x y (z + dz)
      ]

-- | Like 'Data.List.break' but for arrays.
break :: Manifest r e => (e -> Bool) -> Vector r e -> (Vector r e, Vector r e)
break p xs = sliceAt i xs
  where
    i = maybe (size xs) Sz $ findIndex p xs
