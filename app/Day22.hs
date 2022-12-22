-- |
-- Module      :  Main
-- Description :  Day 22; Monkey Map
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/22.
module Main
  ( main,
  )
where

import Control.Applicative (optional, some, (<|>))
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    count,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
  )
import qualified Data.ByteString.Char8 as BS
import Data.Massiv.Array (Array, B, Comp (..), Ix2, fromLists')

data Cell = Tile | Wall | Void
  deriving (Show, Eq)

pCell :: Parser Cell
pCell = tile <|> wall <|> void
  where
    tile = Tile <$ char '.'
    wall = Wall <$ char '#'
    void = Void <$ char ' '

type Field = Array B Ix2 Cell

pLine :: Parser [Cell]
pLine = some pCell

toField :: [[Cell]] -> Field
toField xs = fromLists' Seq $ map elongate xs
  where
    maxLength = maximum $ map length xs
    elongate ys = ys ++ replicate (maxLength - length ys) Void

pField :: Parser Field
pField = toField <$> pLine `sepBy1'` endOfLine

data Turn = TurnR | TurnL
  deriving (Show, Eq)

pTurn :: Parser Turn
pTurn = right <|> left
  where
    right = TurnR <$ char 'R'
    left = TurnL <$ char 'L'

data Instructions = NCons Int Instructions | TCons Turn Instructions | Nil
  deriving (Show, Eq)

pInstructions :: Parser Instructions
pInstructions = pMovementIs <|> pTurnIs

pMovementIs :: Parser Instructions
pMovementIs = NCons <$> decimal <*> pTurnIs <|> pEndIs

pTurnIs :: Parser Instructions
pTurnIs = TCons <$> pTurn <*> pMovementIs <|> pEndIs

pEndIs :: Parser Instructions
pEndIs = Nil <$ endOfLine

pInput :: Parser (Field, Instructions)
pInput = do
  fd <- pField
  _ <- count 2 endOfLine
  is <- pInstructions
  _ <- optional endOfLine
  _ <- endOfInput
  pure (fd, is)

main :: IO ()
main = do
  d <- BS.readFile "inputs/input22-sample.txt"
  let (fd, is) = either error id $ parseOnly pInput d
  print fd
  print is
