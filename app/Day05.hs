{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 5; Crates.
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec  4 10:42:58 2022.
--
-- See https://adventofcode.com/2022/day/5.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.List
import Data.Maybe

type Crate = Char

type Stack = [Crate]

data Move = Move
  { quantity :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

pCrate :: Parser (Maybe Crate)
pCrate =
  Just <$> (char '[' *> anyChar <* char ']')
    <|> string "   " $> Nothing

pCrateLine :: Parser [Maybe Crate]
pCrateLine = pCrate `sepBy1'` char ' '

pPositionsAndWhiteSpace :: Parser [Int]
pPositionsAndWhiteSpace = skipSpace *> decimal `sepBy1` skipSpace <* skipSpace

pCrates :: Parser [[Maybe Crate]]
pCrates = pCrateLine `sepBy1` endOfLine <* endOfLine <* pPositionsAndWhiteSpace

pMove :: Parser Move
pMove = do
  _ <- string "move "
  n <- decimal
  _ <- string " from "
  f <- decimal
  _ <- string " to "
  t <- decimal
  pure $ Move n f t

pMoves :: Parser [Move]
pMoves = pMove `sepBy1'` endOfLine <* skipSpace

data Input = Input
  { crateSetup :: [[Maybe Crate]],
    moves :: [Move]
  }
  deriving (Show)

pInput :: Parser Input
pInput = Input <$> pCrates <*> pMoves <* endOfInput

createSetupToStacks :: [[Maybe Crate]] -> [Stack]
createSetupToStacks xs = map catMaybes $ transpose xs

move :: Move -> [Stack] -> [Stack]
move = undefined

main :: IO ()
main = do
  b <- BS.readFile "inputs/input05.txt"
  let (Input stp mvs) = either error id $ parseOnly pInput b
      stacks = createSetupToStacks stp
  print stacks
