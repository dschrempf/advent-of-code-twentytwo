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
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Maybe
import qualified Data.PartialOrd as P
import qualified Data.Set as S
import Debug.Pretty.Simple

data Valve = Valve
  { name :: String,
    flowRate :: Int,
    tunnels :: [String]
  }
  deriving (Show, Eq, Ord)

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

data State = State
  { minutesLeft :: Int,
    opened :: Valves,
    current :: Valve,
    released :: Int
  }
  deriving (Show, Eq)

instance P.PartialOrd State where
  (State mx ox cx rx) <= (State my oy cy ry)
    -- We only want to compare states at the same time here.
    | mx /= my = error "should not happen"
    | cx == cy = totx < toty
    | otherwise = False
    where
      totx = totalRate ox * mx + rx
      toty = totalRate oy * mx + ry

-- Part 1.
totalRate :: Valves -> Int
totalRate = sum . S.map flowRate

release :: State -> State
release (State m xs c r) = State m xs c $ r + totalRate xs

open :: State -> State
open (State m xs c r) = State (pred m) (S.insert c xs) c r

move :: Valves -> State -> [State]
move vs (State m xs c r) =
  [ State (pred m) xs c' r
    | t <- tunnels c,
      let c' = fromJust $ find ((== t) . name) vs
  ]

lap :: Valves -> State -> [State]
lap vs x = open x' : move vs x'
  where
    x' = release x

clean :: [State] -> [State]
clean = P.maxima

next :: Valves -> [State] -> [State]
next vs xs = case compare (minutesLeft (head xs)) 1 of
  LT -> error "next: out of minutes"
  EQ -> map release xs
  GT -> clean $ concatMap (lap vs) xs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input16-sample.txt"
  let xs = either error id $ parseOnly pInput d
      vs = S.fromList xs
      s0 = State 30 S.empty (head xs) 0
      -- First move.
      ss = move vs s0
      r1 = nTimes 28 (next vs) ss
      r0 = map release r1
  pTraceShowM $ map released r0
