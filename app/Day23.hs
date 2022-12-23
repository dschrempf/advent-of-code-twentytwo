{-# LANGUAGE FlexibleContexts #-}

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
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Massiv.Array
  ( Array,
    B (..),
    Comp (..),
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

pField :: Parser Field
pField = A.fromLists' Par <$> some pTile `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

showField :: Field -> String
showField = unlines . map (map toChar) . A.toLists

-- Part 1.

-- Does the field need to be resized? Also useful to find the final, smallest
-- rectangle.
rowsWithoutElf :: Field -> (Int, Int, Int, Int)
rowsWithoutElf xs = (go West 0, go South 0, go East 0, go North 0)
  where
    (Sz2 rows cols) = A.size xs
    go d n
      | n < rows && n < cols =
          if any isElf (slice d)
            then n
            else go d $ succ n
      | otherwise = n
      where
        slice North = A.delay $ xs !> n
        slice South = A.delay $ xs !> (pred rows - n)
        slice West = xs <! n
        slice East = xs <! (pred cols - n)

shrink :: Int -> Int -> Int -> Int -> Field -> Field
shrink l b r t xs = A.compute $ A.extractFromTo' (Ix2 t l) (Ix2 (rows - b) (cols - r)) xs
  where
    (Sz2 rows cols) = A.size xs

-- > enlarge left bottom right top
enlarge :: Int -> Int -> Int -> Int -> Field -> Field
enlarge l b r t xs = A.compute $ A.concat' 2 [rowsT, xsCols, rowsB]
  where
    (Sz2 rows cols) = A.size xs
    colsL = A.replicate Par (Sz2 rows l) Ground
    colsR = A.replicate Par (Sz2 rows r) Ground
    rowsB = A.replicate Par (Sz2 b (cols + r + l)) Ground
    rowsT = A.replicate Par (Sz2 t (cols + r + l)) Ground
    xsCols = A.computeAs B $ A.concat' 1 [colsL, xs, colsR]

mkPos :: Int -> Int
mkPos n
  | n < 0 = 0
  | otherwise = n

-- Resize the field such that there is enough space for elves to move around.
resize :: Field -> Field
resize xs =
  shrink (sf l) (sf b) (sf r) (sf t) $
    enlarge (ef l) (ef b) (ef r) (ef t) xs
  where
    -- Remove unnecessarily large frames, and reduce frame to 15 fields.
    sf x = mkPos $ x - 20 + 5
    -- Ensure we have a frame of 11 ground fields.
    ef x = mkPos $ 11 - x
    (l, b, r, t) = rowsWithoutElf xs

crop :: Field -> Field
crop xs = shrink l b r t xs
  where
    (l, b, r, t) = rowsWithoutElf xs

data Direction = North | South | West | East

getIxsDir :: Ix2 -> Direction -> [Ix2]
getIxsDir (Ix2 r c) d = case d of
  North -> [Ix2 (pred r) c' | c' <- [pred c, c, succ c]]
  South -> [Ix2 (succ r) c' | c' <- [pred c, c, succ c]]
  West -> [Ix2 r' (pred c) | r' <- [pred r, r, succ r]]
  East -> [Ix2 r' (succ c) | r' <- [pred r, r, succ r]]

getNextIx :: Direction -> Ix2 -> Ix2
getNextIx d (Ix2 r c) = case d of
  North -> Ix2 (pred r) c
  South -> Ix2 (succ r) c
  West -> Ix2 r (pred c)
  East -> Ix2 r (succ c)

-- Map from destination to source positions.
type Moves = M.Map Ix2 (Maybe Ix2)

getFields :: Field -> [Ix2] -> [Tile]
getFields xs is = [xs ! i | i <- is]

getNextFieldDir :: Direction -> Field -> Ix2 -> Maybe Ix2
getNextFieldDir d xs p
  | all isGround (getFields xs nsDir) = Just p'
  | otherwise = Nothing
  where
    p' = getNextIx d p
    nsDir = getIxsDir p d

nextDir :: Direction -> Direction
nextDir North = South
nextDir South = West
nextDir West = East
nextDir East = North

getNextField :: Direction -> Field -> Ix2 -> Maybe Ix2
getNextField d0 xs p@(Ix2 r c)
  | all isGround (getFields xs nsAll) = Nothing
  | otherwise = asum [getNextFieldDir d xs p | d <- take 4 $ iterate nextDir d0]
  where
    nsAll =
      [ Ix2 r' c'
        | r' <- [pred r, r, succ r],
          c' <- [pred c, c, succ c],
          r' /= r || c' /= c
      ]

lockInMoves :: Direction -> Field -> Moves
lockInMoves d xs = A.ifoldlS f M.empty xs
  where
    f m _ Ground = m
    f m p Elf = case getNextField d xs p of
      Nothing -> m
      Just p' -> M.alter (Just . ins p) p' m
    ins x Nothing = Just x
    ins _ _ = Nothing

type FieldM m = MArray (PrimState m) B Ix2 Tile

moveElf :: PrimMonad m => FieldM m -> (Ix2, Ix2) -> m ()
moveElf a (x, y) = A.swap_ a x y

move :: Moves -> Field -> Field
move mvs xs = runST $ do
  a <- A.thawS xs
  mapM_ (moveElf a) mvs'
  A.freezeS a
  where
    -- NOTE: Ugly, but well.
    mvs' = map (second fromJust) $ filter (isJust . snd) $ M.toList mvs

-- Let's resize and crop every round. Slow but who cares.
rnd :: Direction -> Field -> Field
rnd d xs = crop $ move mvs xs'
  where
    xs' = resize xs
    mvs = lockInMoves d xs'

grade :: Field -> Int
grade = length . A.sfilter (== Ground)

-- Part 2.

nRounds :: Direction -> Field -> (Field, Int)
nRounds = go 0
  where
    go n d xs
      | xs == xs' = (xs', succ n)
      | otherwise = go (succ n) (nextDir d) xs'
      where
        xs' = rnd d xs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input23.txt"
  -- Part 1.
  let xs = either error id $ parseOnly pField d
      dirs n = take n $ iterate nextDir North
      xs10 = foldl' (flip rnd) xs (dirs 10)
  putStr $ showField xs10
  print $ grade xs10
  -- Part 2.
  let (xs', n) = nRounds North xs
  putStr $ showField xs'
  print n
