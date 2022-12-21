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

import Control.Applicative (optional, (<|>))
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    choice,
    decimal,
    endOfInput,
    endOfLine,
    isAlpha_ascii,
    parseOnly,
    sepBy1',
    string,
    takeWhile1,
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Tree (Tree (Node))

type Name = BS.ByteString

data MonkeyI
  = MonkeyO
      { arg1 :: Name,
        op :: Int -> Int -> Int,
        arg2 :: Name
      }
  | MonkeyL Int

-- For debugging.
instance Show MonkeyI where
  show (MonkeyO a1 _ a2) = show $ a1 <> " <> " <> a2
  show (MonkeyL n) = show n

pName :: Parser Name
pName = takeWhile1 isAlpha_ascii

pOp :: Parser (Int -> Int -> Int)
pOp = choice [pPl, pMi, pPr, pDi]
  where
    f o c = o <$ char c
    pPl = f (+) '+'
    pMi = f (-) '-'
    pPr = f (*) '*'
    pDi = f div '/'

pMonkeyO :: Parser MonkeyI
pMonkeyO = do
  a1 <- pName
  _ <- char ' '
  op <- pOp
  _ <- char ' '
  a2 <- pName
  pure $ MonkeyO a1 op a2

pMonkeyI :: Parser MonkeyI
pMonkeyI = pMonkeyO <|> (MonkeyL <$> decimal)

pMonkey :: Parser (Name, MonkeyI)
pMonkey = do
  n <- pName
  _ <- string ": "
  i <- pMonkeyI
  pure $ (n, i)

pInput :: Parser [(Name, MonkeyI)]
pInput = pMonkey `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

type Monkeys = M.Map Name MonkeyI

data MonkeyN = Op (Int -> Int -> Int) | Li Int

-- For debugging.
instance Show MonkeyN where
  show (Op _) = "<>"
  show (Li n) = show n

type MonkeyT = Tree MonkeyN

bTree :: Monkeys -> MonkeyT
bTree ms = go r
  where
    r = ms M.! "root"
    go :: MonkeyI -> MonkeyT
    go (MonkeyL n) = Node (Li n) []
    go (MonkeyO x o y) = Node (Op o) [go (ms M.! x), go (ms M.! y)]

cTree :: MonkeyT -> Int
cTree (Node (Li n) []) = n
cTree (Node (Op o) [x, y]) = cTree x `o` cTree y
cTree _ = error "cTree: unknown operation"

main :: IO ()
main = do
  d <- BS.readFile "inputs/input21.txt"
  let ms = either error id $ parseOnly pInput d
      mm = M.fromList ms
      mt = bTree mm
  print $ cTree mt
