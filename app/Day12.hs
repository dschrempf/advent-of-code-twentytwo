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

import Aoc.Array
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Massiv.Array as A
import Data.Maybe
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
pLandscape = fromLists' Seq <$> some pHeight `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- type PathTree = Tree (Ix2, S.Set Ix2)

-- pathTree :: Landscape -> Ix2 -> S.Set Ix2 -> Maybe PathTree
-- pathTree x i v
--   | h == 27 = Just $ Node (i, v) []
--   | otherwise = if null daughters then Nothing else Just $ Node (i, S.singleton i) daughters
--   where
--     h = x ! i
--     daughters =
--       catMaybes
--         [ pathTree x n (S.insert n v)
--           | n <- neighbors (size x) i,
--             n `S.notMember` v,
--             let h' = x ! n,
--             h' <= h + 1
--         ]

data Path = Path
  { path :: [Ix2],
    len :: Int
  }

data State = State
  { paths :: [Path],
    visited :: S.Set Ix2
  }

stepP :: Landscape -> S.Set Ix2 -> Path -> [(Ix2, Path)]
stepP _ _ (Path [] _) = error "stepP: empty path"
stepP xs vs (Path (p : ps) l) =
  [ (n, Path (n : p : ps) (l + 1))
    | n <- neighbors sz p,
      n `S.notMember` vs,
      let h' = xs ! n,
      h' <= h + 1
  ]
  where
    sz = size xs
    h = xs ! p

step :: Landscape -> State -> State
step xs (State ps vs) = State ps' vs'
  where
    (ns', ps') = unzip $ concatMap (stepP xs vs) ps
    vs' = vs `S.union` S.fromList ns'

pocket :: State -> Bool
pocket (State ps _) = 27 `elem` map head ps

main :: IO ()
main = do
  d <- BS.readFile "inputs/input12-sample.txt"
  let x = either error id $ parseOnly pLandscape d
      s = fromJust $ findIndex (== 0) x
      t = pathTree x s (S.singleton s)
  print s
  print t
