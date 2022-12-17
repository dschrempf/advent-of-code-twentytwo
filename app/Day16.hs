{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 16; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/16.
module Main
  ( main,
  )
where

import Aoc.Function
import Control.Applicative
import Control.DeepSeq
import Control.Monad hiding (sequence)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Pretty.Simple
import GHC.Generics
import Prelude hiding (sequence)

data Valve = Valve
  { name :: String,
    flowRate :: Int,
    tunnels :: [String]
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData Valve

pValveName :: Parser String
pValveName = count 2 letter_ascii <?> "vname"

pTunnelString :: Parser ()
pTunnelString =
  void $
    string "; tunnels lead to valves "
      <|> string "; tunnel leads to valve "

pValve :: Parser Valve
pValve = do
  _ <- string "Valve "
  n <- pValveName
  _ <- string " has flow rate="
  r <- decimal
  _ <- pTunnelString
  ts <- pValveName `sepBy1'` string ", "
  pure $ Valve n r ts

type Valves = S.Set Valve

pInput :: Parser [Valve]
pInput = pValve `sepBy1` endOfLine <* optional endOfLine <* endOfInput <?> "input"

-- Part 1.

data State = State
  { opened :: Valves,
    sequence :: [(Int, Valve)],
    current :: Valve,
    prev :: S.Set Valve,
    released :: Int
  }
  deriving (Show, Eq, Generic)

instance NFData State

-- Actually a partial order, but we avoid testing non-compareable elements. If
-- we do so by accident, however, we fail.
compareStates :: Int -> State -> State -> Ordering
compareStates m x y
  | current x /= current y = error "bug: current valves differ"
  | otherwise = compare (released x + yb) (released y + xb)
  where
    xb = backwards m (opened y) $ sequence x
    yb = backwards m (opened x) $ sequence y

-- Calculate released pressure walking the path backwards.
backwards :: Int -> Valves -> [(Int, Valve)] -> Int
backwards m os = snd . foldl' f (0, 0)
  where
    f (nskipped, tot) (n, x)
      | m' >= 30 = (nskipped, tot)
      | x `S.member` os = (succ nskipped, tot)
      | otherwise = (nskipped, releaseAt m' x + tot)
      where
        m' = m + (m - n) + 1 - nskipped

type Map = M.Map Valve [State]

data Trace = Trace
  { _minute :: Int,
    -- Map from current valve to set of attained states.
    _states :: Map
  }
  deriving (Show, Eq, Generic)

instance NFData Trace

-- Total released pressure over the remaining time.
releaseAt :: Int -> Valve -> Int
releaseAt m x = flowRate x * (30 - m)

-- Assume that valve has not been opened yet.
open :: Int -> State -> State
open m (State xs ss c _ r) = State (S.insert c xs) ((m, c) : ss) c (S.singleton c) (r + releaseAt m c)

move :: Valves -> State -> [State]
move vs (State xs ss c p r) =
  [ State xs ss c' p' r
    | t <- tunnels c,
      let c' = fromJust $ find ((== t) . name) vs,
      c' `S.notMember` p,
      let p' = S.insert c' p
  ]

openOrMove :: Int -> Valves -> State -> [State]
openOrMove m vs x@(State xs _ c _ _)
  -- Do not open valves without flow rate.
  | flowRate c == 0 = move vs x
  -- Do not open valve if already open.
  | c `S.member` xs = move vs x
  | otherwise = open m x : move vs x

sortIntoMap :: Int -> [State] -> Map
sortIntoMap m = foldl' insertBetter M.empty
  where
    insertBetter mp x = M.alter (findBetter x) (current x) mp
    findBetter x Nothing = Just [x]
    findBetter x (Just ys) = Just $ case compareStates m x y of
      LT -> y
      EQ -> y
      GT -> x

next :: Valves -> Trace -> Trace
next vs (Trace m xs) = case compare m 30 of
  GT -> error "next: out of minutes"
  _ -> Trace (succ m) $ sortIntoMap m $ concatMap (openOrMove m vs) $ M.elems xs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input16.txt"
  let xs = either error id $ parseOnly pInput d
      vs = S.fromList xs
      x0 = fromJust $ find ((== "AA") . name) xs
      s0 = State S.empty [] x0 (S.singleton x0) 0
      t0 = Trace 1 $ M.singleton x0 s0
      -- First move.
      (Trace _ m) = nTimes 30 (next vs) t0
  pTraceShowM $ maximum $ map released $ M.elems m
