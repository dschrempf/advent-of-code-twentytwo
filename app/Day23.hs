-- |
-- Module      :  Main
-- Description :  Day 23; Unstable Diffusion
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/23.
module Main
  ( main,
  )
where

import Control.Applicative
import Control.Monad.ST (runST)
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Massiv.Array
  ( Array,
    B (..),
    Comp (..),
    D,
    Ix2 (..),
    MArray,
    PrimMonad (..),
    Sz (..),
    (!),
    (!>),
    (<!),
  )
import qualified Data.Massiv.Array as A
import Data.Maybe

data Tile = Elf | Ground
  deriving (Show, Eq)

toChar :: Tile -> Char
toChar Elf = '#'
toChar Ground = '.'

pTile :: Parser Tile
pTile = Ground <$ char '.' <|> Elf <$ char '#'

isElf :: Tile -> Bool
isElf Elf = True
isElf Ground = False
{-# INLINE isElf #-}

isGround :: Tile -> Bool
isGround Elf = False
isGround Ground = True
{-# INLINE isGround #-}

type Field = Array B Ix2 Tile

-- TODO.
-- type FieldD = Array D Ix2 Tile

pField :: Parser Field
pField = A.fromLists' Par <$> some pTile `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

showField :: Field -> String
showField = unlines . map (map toChar) . A.toLists

-- Does the field need to be resized? Also useful to find the final, smallest
-- rectangle.
nRowsWithoutElf :: Field -> Int
nRowsWithoutElf xs = go 0
  where
    (Sz2 rows cols) = A.size xs
    rowsH = rows `div` 2
    colsH = cols `div` 2
    go n
      | n < rowsH && n < colsH =
          if any isElf nRow
            || any isElf sRow
            || any isElf wCol
            || any isElf eCol
            then n
            else go $ succ n
      | otherwise = pred n
      where
        nRow = xs !> 0
        sRow = xs !> pred rows
        wCol = xs <! 0
        eCol = xs <! pred cols

shrink :: Int -> Field -> Field
shrink n xs = A.compute $ A.extractFromTo' (Ix2 n n) (Ix2 (r - n) (c - n)) xs
  where
    (Sz2 r c) = A.size xs

enlarge :: Int -> Field -> Field
enlarge n xs = A.compute $ A.concat' 2 [rows, xsCols, rows]
  where
    (Sz2 r c) = A.size xs
    cols = A.replicate Par (Sz2 r n) Ground
    rows = A.replicate Par (Sz2 n (c + 2 * n)) Ground
    xsCols = A.computeAs B $ A.concat' 1 [cols, xs, cols]

-- Resize the field such that there is enough space for elves to move around.
resize :: Field -> Field
resize xs
  -- Ensure we have a frame of 11 ground fields.
  | n < 11 = enlarge (11 - n) xs
  -- Remove unnecessarily large frames, and reduce frame to 15 fields.
  | n > 20 = shrink (n - 20 + 5) xs
  | otherwise = xs
  where
    n = nRowsWithoutElf xs

data Direction = North | South | West | East

directionGetIxs :: Ix2 -> Direction -> [Ix2]
directionGetIxs (Ix2 r c) d = case d of
  North -> [Ix2 (pred r) c' | c' <- [pred c, c, succ c]]
  South -> [Ix2 (succ r) c' | c' <- [pred c, c, succ c]]
  West -> [Ix2 r' (pred c) | r' <- [pred r, r, succ r]]
  East -> [Ix2 r' (succ c) | r' <- [pred r, r, succ r]]

directionGetIx' :: Ix2 -> Direction -> Ix2
directionGetIx' (Ix2 r c) d = case d of
  North -> Ix2 (pred r) c
  South -> Ix2 (succ r) c
  West -> Ix2 r (pred c)
  East -> Ix2 r (succ c)

-- Map from destination to source positions.
type Moves = M.Map Ix2 (Maybe Ix2)

lockInMoves :: Direction -> Field -> Moves
lockInMoves d xs = A.ifoldlS f M.empty xs
  where
    f m _ Ground = m
    f m p Elf =
      if all isGround [xs ! p' | p' <- directionGetIxs p d]
        then M.alter (Just . ins p) (directionGetIx' p d) m
        else m
    ins x Nothing = Just x
    ins _ _ = Nothing

type FieldM m = MArray (PrimState m) B Ix2 Tile

moveElf :: PrimMonad m => FieldM m -> (Ix2, Ix2) -> m (FieldM m)
moveElf = undefined

move :: Moves -> Field -> Field
move mvs xs = runST $ do
  a <- A.thawS xs
  _ <- foldlM moveElf a mvs'
  A.freezeS a
  where
    -- NOTE: Ugly, but well.
    mvs' = map (second fromJust) $ filter (isJust . snd) $ M.toList mvs

rnd :: Direction -> Field -> Field
rnd d xs = undefined
  where
    mvs = lockInMoves d xs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input23-sample.txt"
  let xs = resize $ either error id $ parseOnly pField d
  putStrLn $ showField xs
  print $ lockInMoves North xs

-- print $ nRowsWithoutElf xs
