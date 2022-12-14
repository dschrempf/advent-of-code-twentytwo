{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day9; Rope Bridge
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Dec  8 23:29:13 2022.
--
-- See https://adventofcode.com/2022/day/9.
module Main
  ( main,
  )
where

import Aoc.Function (nTimesStrict)
import Control.Applicative (Alternative ((<|>)), optional)
import Control.DeepSeq
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HS
import Data.List (foldl', scanl')
import GHC.Generics

-- Part 1.

data Move = U | L | D | R
  deriving (Show, Eq)

data MoveN = MoveN {mvT :: Move, mvN :: Int}
  deriving (Show, Eq)

pMove :: Parser Move
pMove = mWith U 'U' <|> mWith L 'L' <|> mWith D 'D' <|> mWith R 'R'
  where
    mWith o c = o <$ char c

pMoveN :: Parser MoveN
pMoveN = do
  m <- pMove
  _ <- char ' '
  n <- decimal
  pure $ MoveN m n

pInput :: Parser [MoveN]
pInput = pMoveN `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

type Position = (Int, Int)

data State = State
  { positions :: [Position],
    visited :: HS.HashSet Position
  }
  deriving (Show, Eq, Generic)

instance NFData State

-- Follow in one dimension.
followI :: Int -> Int -> Int
followI h t = case compare h t of
  EQ -> t
  GT -> t + 1
  LT -> t - 1

-- Follow in two dimensions.
followP :: Position -> Position -> Position
followP (iH, jH) (iT, jT)
  | d >= 2 = (followI iH iT, followI jH jT)
  | otherwise = (iT, jT)
  where
    dx = abs $ iH - iT
    dy = abs $ jH - jT
    d = max dx dy

moveHead :: Move -> Position -> Position
moveHead m (i, j) = case m of
  U -> (i, j + 1)
  L -> (i - 1, j)
  D -> (i, j - 1)
  R -> (i + 1, j)

-- First move head, then follow.
moveAll :: Move -> [Position] -> [Position]
moveAll m (h : ts) = xs'
  where
    h' = moveHead m h
    xs' = scanl' followP h' ts
moveAll _ [] = error "moveAll: empty rope"

-- Track the positions of the last element.
move :: Move -> State -> State
move m (State xs v) = State xs' v'
  where
    xs' = moveAll m xs
    v' = HS.insert (last xs') v

moveN :: State -> MoveN -> State
moveN s (MoveN m n) = nTimesStrict n (move m) s

-- Part 2.

main :: IO ()
main = do
  d <- BS.readFile "inputs/input09.txt"
  let ms = either error id $ parseOnly pInput d
      p0 = (0, 0)
      s0With n = State (replicate n p0) $ HS.singleton p0
      sfWith n = foldl' moveN (s0With n) ms
  -- Part 1.
  print $ HS.size $ visited $ sfWith 2
  -- Part 2.
  print $ HS.size $ visited $ sfWith 10
