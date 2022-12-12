{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 5; Supply Stacks
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

import Aoc.Function
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
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

-- Part 1.

-- Pop 'Char' from 'Stack' with given index.
pop :: Int -> [Stack] -> (Char, [Stack])
pop i xs = (head x, take j xs ++ [tail x] ++ drop (j + 1) xs)
  where
    j = i - 1
    x = xs !! j

-- Push 'Char' onto 'Stack' with given index.
push :: Int -> Char -> [Stack] -> [Stack]
push i c xs = take j xs ++ [c : x] ++ drop (j + 1) xs
  where
    j = i - 1
    x = xs !! j

move :: Int -> Int -> [Stack] -> [Stack]
move f t xs = let (c, xs') = pop f xs in push t c xs'

moveN :: [Stack] -> Move -> [Stack]
moveN xs (Move n f t) = nTimes n (move f t) xs

moveAll :: [Stack] -> [Move] -> [Stack]
moveAll = foldl' moveN

-- Part 2.

-- Pop @n@ items.
pop' :: Int -> Int -> [Stack] -> (String, [Stack])
pop' n i xs = (take n x, take j xs ++ [drop n x] ++ drop (j + 1) xs)
  where
    j = i - 1
    x = xs !! j

-- Push @n@ items.
push' :: Int -> String -> [Stack] -> [Stack]
push' i s xs = take j xs ++ [s ++ x] ++ drop (j + 1) xs
  where
    j = i - 1
    x = xs !! j

moveN' :: [Stack] -> Move -> [Stack]
moveN' xs (Move n f t) = let (s, xs') = pop' n f xs in push' t s xs'

moveAll' :: [Stack] -> [Move] -> [Stack]
moveAll' = foldl' moveN'

main :: IO ()
main = do
  b <- BS.readFile "inputs/input05.txt"
  let (Input stp mvs) = either error id $ parseOnly pInput b
      stacks = createSetupToStacks stp
  print stacks
  -- Part 1,
  let r1 = moveAll stacks mvs
  print $ map head r1
  -- Part 2.
  let r2 = moveAll' stacks mvs
  print $ map head r2
