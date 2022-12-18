{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Main
-- Description :  Day 18; Boiling Boulders
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/18.
module Main
  ( main,
  )
where

import Aoc.Array
import Aoc.List
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Massiv.Array as A hiding (forM, forM_)
import Prelude as P

type Cube = (Int, Int, Int)

pCube :: Parser Cube
pCube = do
  i <- decimal
  _ <- char ','
  j <- decimal
  _ <- char ','
  k <- decimal
  pure (i, j, k)

pInput :: Parser [Cube]
pInput = pCube `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

diff :: Int -> Int -> Int
diff x y = abs $ x - y

neighbor :: Cube -> Cube -> Bool
neighbor (i1, j1, k1) (i2, j2, k2) = diff i1 i2 + diff j1 j2 + diff k1 k2 <= 1

nNeighbors :: [Cube] -> Int
nNeighbors = length . filter (True ==) . P.map (uncurry neighbor) . pairs

-- Part 1.

totalSurface :: [Cube] -> Int
totalSurface xs = n * 6 - nNeighbors xs * 2
  where
    n = length xs

-- Part 2.

data St = Unknown | Out | Dr | In
  deriving (Eq)

instance Show St where
  show Unknown = "?"
  show Out = " "
  show Dr = "o"
  show In = "."

type Droplet = Array B Ix3 St

type MDroplet s = MArray s B Ix3 St

fillArray :: [Cube] -> Droplet
fillArray cs = runST $ do
  a <- newMArray sz Unknown
  traverse_ (\(x, y, z) -> writeM a (x + 1 :> y + 1 :. z + 1) Dr) cs
  freezeS a
  where
    (xs, ys, zs) = P.unzip3 cs
    xm = maximum xs + 3
    ym = maximum ys + 3
    zm = maximum zs + 3
    sz = Sz $ xm :> ym :. zm

findOneOutside :: Droplet -> Ix3
findOneOutside a =
  head
    [ ix
      | i <- [0 .. x - 1],
        let ix = Ix3 i 0 0,
        (a ! ix) == Unknown
    ]
  where
    (Sz (Ix3 x _ _)) = A.size a

fillOutside :: Droplet -> (Int, Droplet)
fillOutside a = runST $ do
  a' <- thawS a
  n <- fillPositionAndNeighbors a' start
  (n,) <$> freezeS a'
  where
    start = findOneOutside a

fillPositionAndNeighbors ::
  (PrimMonad m, MonadThrow m) =>
  MDroplet (PrimState m) ->
  Ix3 ->
  m Int
fillPositionAndNeighbors a ix = do
  e <- readM a ix
  case e of
    Unknown -> writeM a ix Out >> P.sum <$> forM ns (fillPositionAndNeighbors a)
    Dr -> pure 1
    Out -> pure 0
    In -> error "fillPositionAndNeighbors: magic"
  where
    sz = sizeOfMArray a
    ns = neighborsNoDiagonal3 sz ix

main :: IO ()
main = do
  d <- BS.readFile "inputs/input18.txt"
  let cs = either error id $ parseOnly pInput d
  -- Part 1.
  print $ totalSurface cs
  -- Part 2.
  let a = fillArray cs
      (n, _) = fillOutside a
  print n
