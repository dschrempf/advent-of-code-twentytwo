{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 11; ?
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

import Control.Applicative
import Data.Attoparsec.Text
import Data.Maybe
import qualified Data.Text as TS
import qualified Data.Text.IO as TS
import qualified Data.Vector as V

data Monkey = Monkey
  { items :: [Int],
    _fun :: Int -> Int,
    _test :: Int -> Bool,
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

pTest :: Parser (Int -> Bool)
pTest = f <$> (string "  Test: divisible by " *> decimal)
  where
    f d x = x `mod` d == 0

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
    _monkeys :: V.Vector Monkey
  }

pInput :: Parser State
pInput = State 0 . V.fromList <$> pMonkey `sepBy1'` skipSpace

throw1 :: Int -> V.Vector Monkey -> V.Vector Monkey
throw1 n ms = case ms V.! n of
  monkeyN@(Monkey (i : is) f t tr fa) ->
    let monkeyN' = monkeyN {items = is}
        newWorry = f i `div` 3
        m = if t newWorry then tr else fa
        -- Assume that monkeys do not throw to themselves.
        monkeyM@(Monkey isM _ _ _ _) = ms V.! m
        -- This is slow, but who cares.
        monkeyM' = monkeyM {items = isM ++ [i]}
     in ms V.// [(n, monkeyN'), (m, monkeyM')]
  _ -> error "throw1: empty monkey"

throw :: State -> [State]
throw (State i ms)
  | stop = [x']
  | otherwise = x' : throw x'
  where
    is = items $ ms V.! i
    nMs = V.length ms
    nIs = length is
    (i', stop) = if nIs == 1 then (i + 1 `mod` nMs, i == nMs - 1) else (i, False)
    x' = State i' $ throw1 i' ms

main :: IO ()
main = do
  d <- TS.readFile "inputs/input11.txt"
  let s = either error id $ parseOnly pInput d
      t = throw s
  print $ map current t
