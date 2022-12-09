{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day9; ?
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

import Aoc.Function
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (D)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HS
import Data.List

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
  { positionHead :: Position,
    positionTail :: Position,
    visited :: HS.HashSet Position
  }
  deriving (Show, Eq)

followI :: Int -> Int -> Int
followI h t = case compare h t of
  EQ -> t
  GT -> t + 1
  LT -> t - 1

followP :: Position -> Position -> Position
followP (iH, jH) (iT, jT)
  | d >= 2 = (followI iH iT, followI jH jT)
  | otherwise = (iT, jT)
  where
    dx = abs $ iH - iT
    dy = abs $ jH - jT
    d = max dx dy

move :: Move -> State -> State
move m (State (i, j) t v) = State h' t' v'
  where
    h' = case m of
      U -> (i, j + 1)
      L -> (i - 1, j)
      D -> (i, j - 1)
      R -> (i + 1, j)
    t' = followP h' t
    v' = HS.insert t' v

moveN :: State -> MoveN -> State
moveN s (MoveN m n) = nTimes n (move m) s

-- Part 2.

main :: IO ()
main = do
  d <- BS.readFile "inputs/input09.txt"
  let ms = either error id $ parseOnly pInput d
      p0 = (0, 0)
      s0 = State p0 p0 $ HS.singleton p0
      sf = foldl' moveN s0 ms
  print $ HS.size $ visited sf
