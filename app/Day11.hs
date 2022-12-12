{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 11; Monkey in the Middle
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/11.
module Main
  ( main,
  )
where

import Aoc.Occurrence (count)
import Control.Applicative (Alternative ((<|>)), optional)
import Data.Attoparsec.Text
  ( Parser,
    char,
    decimal,
    endOfLine,
    parseOnly,
    sepBy1,
    sepBy1',
    skipSpace,
    string,
  )
import Data.Bifunctor (Bifunctor (first))
import Data.List (sort, unfoldr)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as TS
import qualified Data.Text.IO as TS
import qualified Data.Vector as V

data Monkey = Monkey
  { items :: [Int],
    _fun :: Int -> Int,
    test :: Int,
    _throwT :: Int,
    _throwF :: Int
  }

pItems :: Parser [Int]
pItems = string "  Starting items: " *> decimal `sepBy1` string ", "

pFun :: Parser (Int -> Int)
pFun = do
  _ <- string "  Operation: new = "
  a1 <- pVar
  _ <- char ' '
  op <- pOp
  _ <- char ' '
  a2 <- pVar
  pure $ f a1 a2 op
  where
    pVar = (Nothing <$ string "old") <|> (Just <$> decimal)
    pOp = ((+) <$ char '+') <|> ((*) <$ char '*')
    f :: Maybe Int -> Maybe Int -> (Int -> Int -> Int) -> (Int -> Int)
    f m1 m2 o x = fromMaybe x m1 `o` fromMaybe x m2

pTest :: Parser Int
pTest = string "  Test: divisible by " *> decimal

pThrowTo :: TS.Text -> Parser Int
pThrowTo w = string "    If " *> string w *> string ": throw to monkey " *> decimal

pMonkey :: Parser Monkey
pMonkey = do
  _ <- string "Monkey " *> (decimal :: Parser Int) <* char ':' <* endOfLine
  xs <- pItems <* endOfLine
  f <- pFun <* endOfLine
  t <- pTest <* endOfLine
  tt <- pThrowTo "true" <* endOfLine
  tf <- pThrowTo "false" <* optional endOfLine
  pure $ Monkey xs f t tt tf

data State = State
  { current :: Int,
    monkeys :: V.Vector Monkey
  }

pInput :: Parser State
pInput = State 0 . V.fromList <$> pMonkey `sepBy1'` skipSpace

throw1 ::
  -- Part 1: Nothing, Part 2: Just product of divisors to keep worries bounded.
  Maybe Int ->
  -- Which monkey throws?
  Int ->
  V.Vector Monkey ->
  V.Vector Monkey
throw1 decreaseWorry n ms = case ms V.! n of
  monkeyN@(Monkey (i : is) f t tr fa) ->
    let monkeyN' = monkeyN {items = is}
        newWorry = case decreaseWorry of
          Nothing -> f i `div` 3
          Just x -> f i `mod` x
        testFun x = x `mod` t == 0
        m = if testFun newWorry then tr else fa
        -- Assume that monkeys do not throw to themselves.
        monkeyM@(Monkey isM _ _ _ _) = ms V.! m
        -- This is slow, but who cares.
        monkeyM' = monkeyM {items = isM ++ [newWorry]}
     in ms V.// [(n, monkeyN'), (m, monkeyM')]
  _ -> error "throw1: empty monkey"

throw ::
  -- Part 1: Nothing, Part 2: Just product of divisors to keep worries bounded.
  Maybe Int ->
  State ->
  -- Return type fits 'unfoldr'.
  Maybe ([State], State)
throw decreaseWorry (State i ms)
  -- Do not throw, change to first monkey, stop.
  | nIs == 0 && i' == 0 = let x' = State i' ms in Just ([], x')
  -- Do not throw, change to next monkey.
  | nIs == 0 = let x' = State i' ms in throw decreaseWorry x'
  -- Throw, do not change to next monkey.
  | otherwise = let x' = State i $ throw1 decreaseWorry i ms in first (x' :) <$> throw decreaseWorry x'
  where
    nMs = V.length ms
    is = items $ ms V.! i
    nIs = length is
    i' = succ i `mod` nMs

main :: IO ()
main = do
  d <- TS.readFile "inputs/input11.txt"
  -- Part 1.
  let s = either error id $ parseOnly pInput d
      rs1 = take 20 $ unfoldr (throw Nothing) s
      ms1 = count $ map current $ concat rs1
      mb1 = product $ take 2 $ reverse $ sort $ map snd $ M.toList ms1
  print ms1
  print mb1
  -- Part 2.
  --
  -- Find product of divisors.
  let pdiv = V.product $ V.map test $ monkeys s
  let rs2 = take 10000 $ unfoldr (throw (Just pdiv)) s
      ms2 = count $ map current $ concat rs2
      mb2 = product $ take 2 $ reverse $ sort $ map snd $ M.toList ms2
  print ms2
  print mb2
