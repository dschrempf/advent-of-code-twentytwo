{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 10; Cathode-Ray Tube
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/10.
module Main
  ( main,
  )
where

import Aoc.List (chop)
import Control.Applicative (Alternative ((<|>)), optional)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
    signed,
    string,
  )
import qualified Data.ByteString.Char8 as BS
import Data.List (foldl')

newtype X = X {getX :: Int}
  deriving (Show, Eq)
  deriving (Num) via Int

data Instruction = Noop | Addx Int
  deriving (Show, Eq)

pInstruction :: Parser Instruction
pInstruction = noop <|> addx
  where
    noop = Noop <$ string "noop"
    addx = Addx <$> (string "addx " *> signed decimal)

pInput :: Parser [Instruction]
pInput = pInstruction `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- Part 1.

evalStep :: [X] -> Instruction -> [X]
evalStep [] _ = error "evalStep: empty register"
evalStep (x : xs) Noop = x : x : xs
-- Addition adds two cycles.
evalStep (x : xs) (Addx n) = (x + X n) : x : x : xs

signalStrengths :: [X] -> [Int]
signalStrengths xs = [getX $ X i * (xs !! pred i) | i <- [20, 60 .. l]]
  where
    l = length xs

draw :: Int -> X -> Char
draw n (X m)
  | n == pred m || n == m || n == succ m = '#'
  | otherwise = '.'

drawAll :: [X] -> [String]
drawAll xs = [zipWith draw [0 .. 40] ln | ln <- lns]
  where
    lns = chop 40 xs

main :: IO ()
main = do
  b <- BS.readFile "inputs/input10.txt"
  -- Part 1.
  let is = either error id $ parseOnly pInput b
      cs = reverse $ foldl' evalStep [X 1] is
      ss = signalStrengths cs
  print $ sum ss
  -- Part 2.
  --
  -- Somehow we end up with an additional '.' at the end.
  mapM_ print $ drawAll cs
