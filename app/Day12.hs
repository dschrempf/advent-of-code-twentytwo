-- |
-- Module      :  Main
-- Description :  Day 12; Hill Climbing Algorithm
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/12.
module Main
  ( main,
  )
where

import Aoc.Array (neighborsNoDiagonal)
import Control.Applicative (Alternative (some), optional)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    endOfInput,
    endOfLine,
    letter_ascii,
    parseOnly,
    sepBy1',
  )
import qualified Data.ByteString.Char8 as BS
import Data.Char (isLower, ord)
import Data.List (find, nubBy)
import qualified Data.Massiv.Array as A
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S

type Landscape = A.Array A.U A.Ix2 Int

cToI :: Char -> Int
cToI 'S' = 0
cToI 'E' = 27
cToI c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = error $ "cToI: unknown char: " <> show c

pHeight :: Parser Int
pHeight = cToI <$> letter_ascii

pLandscape :: Parser Landscape
pLandscape = A.fromLists' A.Seq <$> some pHeight `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- First, I tried to get the paths as a tree, but this was too slow.
type Path = [A.Ix2]

data State = State
  { paths :: [Path],
    visited :: S.Set A.Ix2
  }
  deriving (Show)

stepP ::
  Landscape ->
  S.Set A.Ix2 ->
  Path ->
  -- @[(new field, new path)]@.
  [(A.Ix2, Path)]
stepP _ _ [] = error "stepP: empty path"
stepP xs vs (p : ps) =
  [ (n, n : p : ps)
    | n <- neighborsNoDiagonal sz p,
      n `S.notMember` vs,
      let h' = xs A.! n,
      h' <= h + 1
  ]
  where
    sz = A.size xs
    h = xs A.! p

step :: Landscape -> State -> State
step xs (State ps vs) = State ps'' vs'
  where
    (ns', ps') = unzip $ concatMap (stepP xs vs) ps
    -- Only keep one path to the current field.
    ps'' = nubBy (\x y -> head x == head y) ps'
    vs' = vs `S.union` S.fromList ns'

pocket :: Landscape -> State -> Bool
pocket xs (State ps _) = isJust $ find p $ map head ps
  where
    p i = (xs A.! i) == 27

findShortestPath :: Landscape -> State -> Int
findShortestPath xs =
  pred
    . length
    -- One path is enough (actually, 'nubBy' already ensures that only one path
    -- is left in the list).
    . head
    . paths
    . fromJust
    . find (pocket xs) -- Stop when reaching E.
    . iterate (step xs) -- Step around on the grid.

findShortestPath1 :: Landscape -> A.Ix2 -> Int
findShortestPath1 xs i0 = findShortestPath xs s0
  where
    ps0 = [[i0]]
    v0 = S.singleton i0
    s0 = State ps0 v0

-- One could go from the end to the first start. Here, we start at all possible
-- start positions and go to the end.
findShortestPath2 :: Landscape -> [A.Ix2] -> Int
findShortestPath2 xs is0 = findShortestPath xs s0
  where
    ps0 = [[i] | i <- is0]
    v0 = S.fromList is0
    s0 = State ps0 v0

main :: IO ()
main = do
  d <- BS.readFile "inputs/input12.txt"
  let xs = either error id $ parseOnly pLandscape d
      i0 = fromJust $ A.findIndex (== 0) xs
  -- Part 1.
  print $ findShortestPath1 xs i0
  -- Part 2.
  let is0 = A.fold $ A.imap (\ix e -> [ix | e == 0 || e == 1]) xs
      ls = findShortestPath2 xs is0
  print ls
