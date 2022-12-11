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

data Monkey = Monkey
  { items :: [Int],
    fun :: Int -> Int,
    test :: Int -> Bool,
    throwT :: Int,
    throwF :: Int
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

pMonkey :: Parser (Int, Monkey)
pMonkey = do
  n <- string " Monkey " *> decimal <* char ':' <* endOfLine
  xs <- pItems <* endOfLine
  f <- pFun <* endOfLine
  t <- pTest <* endOfLine
  tt <- pThrowTo "true" <* endOfLine
  tf <- pThrowTo "false" <* optional endOfLine
  pure (n, Monkey xs f t tt tf)

main :: IO ()
main = undefined
