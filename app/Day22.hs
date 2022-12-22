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

import Aoc.Function (nTimesStrict)
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
import Data.Massiv.Array
  ( Array,
    B,
    Comp (..),
    Ix2 (..),
    U (..),
    Vector,
    compute,
    findIndex,
    fromList,
    fromLists',
    toList,
    (!),
    (!+!),
    (!><),
  )
import qualified Data.Massiv.Array as A
import Data.Maybe (fromJust)

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
  xs <- pField
  _ <- count 2 endOfLine
  is <- pInstructions
  _ <- optional endOfLine
  _ <- endOfInput
  pure (xs, is)

-- Part 1.

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
wrap field direction position = go position field direction position
  where
    go :: Position -> Field -> Direction -> Position -> Position
    go p0 xs d p = case x' of
      -- Need to check if this is a wall. If so, provide the original position.
      Void -> if (xs ! p) == Wall then p0 else p
      _ -> go p0 xs d p'
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

move :: Field -> Instructions -> Direction -> Position -> (Direction, Position)
move xs (NCons n is') d p = move xs is' d $ nTimesStrict n (moveOne xs d) p
move xs (TCons t is') d p = move xs is' (turn t d) p
move _ Nil d p = (d, p)

findStart :: Field -> Position
findStart = fromJust . findIndex (== Tile)

grade :: Direction -> Position -> Int
grade d p = gd d + gp p
  where
    gd DRight = 0
    gd DDown = 1
    gd DLeft = 2
    gd DUp = 3
    gp (Ix2 i j) = 1000 * i + 4 * j

-- Part 2.

type V = Vector U Int

type M = Array U Ix2 Int

-- Position.
type P = V

froml :: [Int] -> V
froml = fromList Seq

fromls :: [[Int]] -> M
fromls = fromLists' Seq

pos1 :: Turn -> Int
pos1 TRight = 1
pos1 TLeft = -1

neg1 :: Turn -> Int
neg1 TRight = -1
neg1 TLeft = 1

rotx :: Turn -> M
rotx t =
  fromls
    [ [1, 0, 0],
      [0, 0, neg1 t],
      [0, pos1 t, 0]
    ]

roty :: Turn -> M
roty t =
  fromls
    [ [0, 0, pos1 t],
      [0, 1, 0],
      [neg1 t, 0, 0]
    ]

rotz :: Turn -> M
rotz t =
  fromls
    [ [0, neg1 t, 0],
      [pos1 t, 0, 0],
      [0, 0, 1]
    ]

flipt :: Turn -> Turn
flipt TRight = TLeft
flipt TLeft = TRight

getTurn :: Turn -> V -> M
getTurn t o = case toList o of
  [1, 0, 0] -> rotx t
  [-1, 0, 0] -> rotx $ flipt t
  [0, 1, 0] -> roty t
  [0, -1, 0] -> roty $ flipt t
  [0, 0, 1] -> rotz t
  [0, 0, -1] -> rotz $ flipt t
  _ -> error $ "getTurn: unknown orientation: " ++ show o

data Walker = Walker
  { _direction :: V,
    _orientation :: V
  }
  deriving (Show, Eq)

turnw :: Turn -> Walker -> Walker
turnw t (Walker d o) = Walker d' o
  where
    d' = compute $ getTurn t o !>< d

cross :: V -> V -> V
cross a b = froml [a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1]
  where
    [a1, a2, a3] = toList a
    [b1, b2, b3] = toList b

-- Flip walker over an edge.
flipw :: Walker -> Walker
flipw (Walker d o) = Walker (t `g` d) (t `g` o)
  where
    f = cross d o
    t = getTurn TLeft f
    g m v = compute $ m !>< v

w :: Walker
w = Walker (froml [1, 0, 0]) (froml [0, 0, 1])

movew :: Walker -> P -> P
movew (Walker d _) p = p !+! d

main :: IO ()
main = do
  d <- BS.readFile "inputs/input22.txt"
  -- Part 1.
  let (xs, is) = either error id $ parseOnly pInput d
      x0 = findStart xs
  print $ uncurry grade $ move xs is DRight x0
  -- Part 2.
  undefined
