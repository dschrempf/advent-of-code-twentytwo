{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 21; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/21.
module Main
  ( main,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    choice,
    decimal,
    isAlpha_ascii,
    string,
    takeWhile1,
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Tree (Tree)

data MonkeyIn
  = MonkeyOp
      { arg1 :: BS.ByteString,
        op :: Int -> Int -> Int,
        arg2 :: BS.ByteString
      }
  | MonkeyLi Int

data Monkey = Monkey BS.ByteString MonkeyIn

pName :: Parser BS.ByteString
pName = takeWhile1 isAlpha_ascii

pOp :: Parser (Int -> Int -> Int)
pOp = choice [pPl, pMi, pPr, pDi]
  where
    f o c = o <$ char c
    pPl = f (+) '+'
    pMi = f (-) '-'
    pPr = f (*) '*'
    pDi = f div '/'

pMonkeyOp :: Parser MonkeyIn
pMonkeyOp = do
  a1 <- pName
  _ <- char ' '
  op <- pOp
  _ <- char ' '
  a2 <- pName
  pure $ MonkeyOp a1 op a2

pMonkeyIn :: Parser MonkeyIn
pMonkeyIn = pMonkeyOp <|> (MonkeyLi <$> decimal)

pMonkey :: Parser Monkey
pMonkey = do
  n <- pName
  _ <- string ": "
  undefined

type Monkeys = M.Map BS.ByteString MonkeyIn

-- data MonkeyO
--   = Op (Int -> Int -> Int)
--   | Id String
--   | Li Int

-- type MTree = Tree MonkeyO

main :: IO ()
main = undefined
