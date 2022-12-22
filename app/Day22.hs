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
import Data.Massiv.Array (Array, B, Comp (..), Ix2 (..), fromLists', (!))

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

-- Also, add a 'Void' frame; see 'wrap'.
toField :: [[Cell]] -> Field
toField xs = fromLists' Seq $ frameLine : map elongate xs ++ [frameLine]
  where
    maxLength = maximum $ map length xs
    elongate ys = Void : ys ++ replicate (maxLength + 1 - length ys) Void
    frameLine = replicate (maxLength + 2) Void

pField :: Parser Field
pField = toField <$> pLine `sepBy1'` endOfLine

data Turn = TRight | TLeft
  deriving (Show, Eq)

pTurn :: Parser Turn
pTurn = right <|> left
  where
    right = TRight <$ char 'R'
    left = TLeft <$ char 'L'

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

type Position = Ix2

data Direction = DLeft | DDown | DRight | DUp
  deriving (Show, Eq, Bounded, Enum)

forwards :: Direction -> Position -> Position
forwards d (Ix2 i j) = case d of
  DLeft -> i :. pred j
  DDown -> succ i :. j
  DRight -> i :. succ j
  DUp -> pred i :. j

opposite :: Direction -> Direction
opposite DLeft = DRight
opposite DRight = DLeft
opposite DUp = DDown
opposite DDown = DUp

backwards :: Direction -> Position -> Position
backwards = forwards . opposite

-- Dragons.
--
-- Move backwards until finding 'Void'. We can do this since we framed the board
-- with 'Void'.
wrap :: Field -> Direction -> Position -> Position
wrap xs d p = case x' of
  Void -> p
  _ -> wrap xs d p'
  where
    p' = backwards d p
    x' = xs ! p'

moveOne :: Field -> Direction -> Position -> Position
moveOne xs d p = case x' of
  Wall -> p
  Tile -> p'
  Void -> wrap xs d p
  where
    p' = forwards d p
    x' = xs ! p'

turn :: Turn -> Direction -> Direction
turn TRight DLeft = DUp
turn TRight d = pred d
turn TLeft DUp = DLeft
turn TLeft d = succ d

main :: IO ()
main = do
  d <- BS.readFile "inputs/input22-sample.txt"
  let (fd, is) = either error id $ parseOnly pInput d
  print fd
  print is
