{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 16; Proboscidea Volcanium
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

data Path = Path
  { opened :: Valves,
    sequence :: [(Int, Valve)],
    current :: Valve,
    prev :: S.Set Valve,
    released :: Int
  }
  deriving (Show, Eq, Generic)

instance NFData Path

-- Actually a partial order, but we avoid testing non-compareable elements. If
-- we do so by accident, we fail.
comparePaths :: Int -> Int -> Path -> Path -> Ordering
comparePaths mnow mmax x y
  | current x /= current y = error "comparePaths: bug: current valves differ"
  | otherwise = compare (gradePath mnow mmax x y) (gradePath mnow mmax y x)

gradePath :: Int -> Int -> Path -> Path -> Int
gradePath mnow mmax x y = released x + yb
  where
    yb = backwards mnow mmax (opened x) $ sequence y

-- Calculate released pressure walking the path backwards.
backwards ::
  -- Current minute.
  Int ->
  -- Maximum minutes.
  Int ->
  -- Opened valves.
  Valves ->
  -- Sequence of other path.
  [(Int, Valve)] ->
  Int
backwards mnow mmax os = snd . foldl' f (0, 0)
  where
    f (nskipped, tot) (n, x)
      | mnow' >= mmax = (nskipped, tot)
      | x `S.member` os = (succ nskipped, tot)
      | otherwise = (nskipped, releaseAt mnow' mmax x + tot)
      where
        mnow' = mnow + (mnow - n) + 1 - nskipped

-- Map from current valve to list of equivalent paths.
type Map = M.Map Valve [Path]

data Trace = Trace
  { _minute :: Int,
    _paths :: Map
  }
  deriving (Show, Eq, Generic)

instance NFData Trace

-- Total released pressure over the remaining time.
releaseAt :: Int -> Int -> Valve -> Int
releaseAt mnow mmax x = flowRate x * (mmax - mnow)

-- Assume the valve has not been opened yet. Also reset previously visited paths
-- to enable movement in backward direction.
open :: Int -> Int -> Path -> Path
open mnow mmax (Path xs ss c _ r) =
  Path (S.insert c xs) ((mnow, c) : ss) c (S.singleton c) (r + releaseAt mnow mmax c)

move :: Valves -> Path -> [Path]
move vs (Path xs ss c p r) =
  [ Path xs ss c' p' r
    | t <- tunnels c,
      let c' = fromJust $ find ((== t) . name) vs,
      -- Only move to valves not visited since the last valve was opened.
      c' `S.notMember` p,
      let p' = S.insert c' p
  ]

openOrMove :: Int -> Int -> Valves -> Path -> [Path]
openOrMove mnow mmax vs x@(Path xs _ c _ _)
  -- Do not open valves without flow rate.
  | flowRate c == 0 = move vs x
  -- Do not open valves already open.
  | c `S.member` xs = move vs x
  | otherwise = open mnow mmax x : move vs x

sortIntoMap :: Int -> Int -> [Path] -> Map
sortIntoMap mnow mmax = foldl' insertBetter M.empty
  where
    insertBetter mp x = M.alter (Just . findBetter x) (current x) mp
    findBetter x Nothing = [x]
    findBetter x (Just ys)
      -- If the new path is better than all others, use it exclusively.
      | all (== GT) cs = [x]
      -- If the new path is better than some others, use the best paths.
      | GT `elem` cs = x : catMaybes (zipWith p cs ys)
      | otherwise = ys
      where
        cs = map (comparePaths mnow mmax x) ys
    p GT _ = Nothing
    p _ y = Just y

next :: Int -> Valves -> Trace -> Trace
next mmax vs (Trace mnow xs) =
  case compare mnow mmax of
    GT -> error "next: out of minutes"
    _ -> Trace (succ mnow)
    $ sortIntoMap mnow mmax
    $ concatMap (openOrMove mnow mmax vs)
    $ concat
    $ M.elems xs

-- Part 2.

data Path2 = Path2 Path Path
  deriving (Show, Eq, Generic)

instance NFData Path2

comparePaths2 :: Int -> Int -> Path2 -> Path2 -> Ordering
comparePaths2 mnow mmax (Path2 x1 x2) (Path2 y1 y2)
  | current x1 /= current y1 = error "comparePath2: bug: current valves of path 1 differ"
  | current x2 /= current y2 = error "comparePath2: bug: current valves of path 2 differ"
  | opened x1 `S.isSubsetOf` opened y1 && (released x1 + released x2 >= released y1 + released y2) = GT
  | opened y1 `S.isSubsetOf` opened x1 && (released y1 + released y2 >= released x1 + released x2) = LT
  | otherwise = case (comparePaths mnow mmax x1 y1, comparePaths mnow mmax x2 y2) of
      (GT, GT) -> GT
      (LT, LT) -> LT
      (_, _) -> EQ

-- Gives 2651 which is still too low.
--
-- \| otherwise = compare (f x1 y1 + f x2 y2) (f y1 x1 + f y2 x2)
-- where
--   f = gradePath mnow mmax

type Map2 = M.Map (Valve, Valve) [Path2]

data Trace2 = Trace2
  { _minute2 :: Int,
    _paths2 :: Map2
  }
  deriving (Show, Eq, Generic)

instance NFData Trace2

openOrMove2 :: Int -> Int -> Valves -> Path2 -> [Path2]
openOrMove2 mnow mmax vs p@(Path2 x y)
  -- Wait if all valves are open.
  | vsWithFlow == opened x = [p]
  | otherwise = ps2'
  where
    xs = openOrMove mnow mmax vs x
    ys = openOrMove mnow mmax vs y
    ps2' =
      [ Path2 a'' b''
        | a <- xs,
          let ao = lastOpened x a,
          b <- ys,
          let bo = lastOpened y b,
          -- If both paths have opened a valve, ensure it has not been the same one.
          not $ isJust ao && (ao == bo),
          let (a', b') = if current a <= current b then (a, b) else (b, a),
          let os = opened a' `S.union` opened b',
          let a'' = a' {opened = os},
          let b'' = b' {opened = os}
      ]
    vsNoFlow = S.filter ((==) 0 . flowRate) vs
    vsWithFlow = vs `S.difference` vsNoFlow

lastOpened :: Path -> Path -> Maybe Valve
lastOpened x y
  | released x < released y = Just $ snd $ head $ sequence y
  | otherwise = Nothing

sortIntoMap2 :: Int -> Int -> [Path2] -> Map2
sortIntoMap2 mnow mmax = foldl' insertBetter M.empty
  where
    insertBetter mp x@(Path2 a b) = M.alter (Just . findBetter x) (current a, current b) mp
    findBetter x Nothing = [x]
    findBetter x (Just ys)
      -- If the new path is better than all others, use it exclusively.
      | all (== GT) cs = [x]
      -- If the new path is better than some others, use the best paths.
      | EQ `elem` cs = x : catMaybes (zipWith p cs ys)
      | otherwise = ys
      where
        cs = map (comparePaths2 mnow mmax x) ys
    p GT _ = Nothing
    p _ y = Just y

next2 :: Int -> Valves -> Trace2 -> Trace2
next2 mmax vs (Trace2 mnow xs) =
  pTraceShow mnow
    $ case compare mnow mmax of
      GT -> error "next: out of minutes"
      _ -> Trace2 (succ mnow)
    $ sortIntoMap2 mnow mmax
    $ concatMap (openOrMove2 mnow mmax vs)
    $ concat
    $ M.elems xs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input16.txt"
  -- Part 1.
  let xs = either error id $ parseOnly pInput d
      vs = S.fromList xs
      x0 = fromJust $ find ((== "AA") . name) xs
      p0 = Path S.empty [] x0 (S.singleton x0) 0
      t0 = Trace 1 $ M.singleton x0 [p0]
      (Trace _ m) = nTimes 30 (next 30 vs) t0
  pTraceShowM $ maximum $ map released $ concat $ M.elems m
  -- Part 2.
  let p20 = Path2 p0 p0
      t20 = Trace2 1 $ M.singleton (x0, x0) [p20]
      (Trace2 _ m2) = nTimes 26 (next2 26 vs) t20
      ys = concat $ M.elems m2
  pTraceShowM $ maximum $ map (\(Path2 a b) -> released a + released b) ys

-- pTraceShowM $ maximumBy (\(Path2 a b) (Path2 c d) -> compare (released a + released b) (released c + released d)) ys
