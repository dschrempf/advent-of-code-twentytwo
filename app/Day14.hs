{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Main
-- Description :  Day 14; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/14.
module Main
  ( main,
  )
where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import Control.Applicative
import Data.List

type Ix = (Int, Int)

type Cave = S.Set Ix

line :: Ix -> Ix -> [Ix]
line (x0, y0) (x1, y1)
  | x0 == x1 = [(x0, y) | y <- [y0 .. y1] ++ [y1 .. y0]]
  | y0 == y1 = [(x, y0) | x <- [x0 .. x1] ++ [x1 .. x0]]
  | otherwise = error "getLine: no straight line"

addRockLine :: Ix -> Ix -> Cave -> Cave
addRockLine f t c = foldl' (flip S.insert) c $ line f t

addRock :: [Ix] -> Cave -> Cave
addRock [] c = c
addRock (_:[]) c = c
addRock (x:y:xs) c = let c' = addRockLine x y c in addRock (y:xs) c'

addRocks :: [Rock] -> Cave
addRocks = foldl' (flip addRock) S.empty

type Rock = [Ix]

pIx2 :: Parser Ix
pIx2 = do
  x <- decimal
  _ <- char ','
  y <- decimal
  pure $ (x, y)

pRock :: Parser Rock
pRock = pIx2 `sepBy1'` string " -> "

pInput :: Parser [Rock]
pInput = pRock `sepBy1'` skipSpace <* optional skipSpace <* endOfInput

down :: Ix -> Ix
down (x, y) = (x, succ y)

dleft :: Ix -> Ix
dleft (x, y) = (pred x, succ y)

dright :: Ix -> Ix
dright (x, y) = (succ x, succ y)

next :: Ix -> Int -> Cave -> Maybe Ix
next p m c
  | snd p > m = Nothing
  | d `S.notMember` c = next d m c
  | l `S.notMember` c = next l m c
  | r `S.notMember` c = next r m c
  | otherwise = Just p
  where
    d = down p
    l = dleft p
    r = dright p

pour ::
  -- Y value of lowest rock position.
  Int ->
  Cave ->
  Cave
pour m c = case next (500, 0) m c of
          Nothing -> c
          (Just p) -> pour m $ p `S.insert` c

next2 :: Ix -> Int -> Cave -> Maybe Ix
next2 p m c
  | snd p + 1 == m = Just p
  | d `S.notMember` c = next2 d m c
  | l `S.notMember` c = next2 l m c
  | r `S.notMember` c = next2 r m c
  | p == (500, 0) = Nothing
  | otherwise = Just p
  where
    d = down p
    l = dleft p
    r = dright p

pour2 ::
  -- Y value of lowest rock position.
  Int ->
  Cave ->
  Cave
pour2 m c = case next2 (500, -1) m c of
          Nothing -> c
          (Just p) -> pour2 m $ p `S.insert` c

main :: IO ()
main = do
  d <- BS.readFile "inputs/input14.txt"
  -- Part 1.
  let rocks = either error id $ parseOnly pInput d
      cave = addRocks rocks
      ymax = S.findMax $ S.map snd cave
      cave1 = pour ymax cave
  print $ S.size cave1 - S.size cave
  -- Part 2.
  let flr = ymax + 2
      cave2 = pour2 flr cave
  print $ succ $ S.size cave2 - S.size cave
