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
    (!><),
    (!><!),
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

froml :: [Int] -> V
froml = fromList Seq

fromls :: [[Int]] -> M
fromls = fromLists' Seq

idm :: M
idm =
  fromls
    [ [1, 0, 0],
      [0, 1, 0],
      [0, 0, 1]
    ]

ng :: Int -> Int
ng = negate

rotx :: Int -> M
rotx 0 = idm
rotx x =
  fromls
    [ [1, 0, 0],
      [0, 0, ng x],
      [0, x, 0]
    ]

roty :: Int -> M
roty 0 = idm
roty x =
  fromls
    [ [0, 0, x],
      [0, 1, 0],
      [ng x, 0, 0]
    ]

rotz :: Int -> M
rotz 0 = idm
rotz x =
  fromls
    [ [0, ng x, 0],
      [x, 0, 0],
      [0, 0, 1]
    ]

rot :: Int -> Int -> Int -> M
rot x y z = rotx x !><! roty y !><! rotz z

turnv :: V -> V -> V
turnv t v = compute $ rot x y z !>< v
  where
    [x, y, z] = toList t

turnr, turnl :: M
turnr = rot 0 0 1
turnl = rot 0 0 (-1)

getpm :: V -> M
getpm v = case toList v of
  [1, 0, 0] -> rot 0 (-1) 0
  [-1, 0, 0] -> rot 0 1 0
  [0, 1, 0] -> rot 1 0 0
  [0, -1, 0] -> rot (-1) 0 0
  [0, 0, -1] -> rot (-1) (-1) (-1)
  [0, 0, 1] -> idm
  _ -> error "getpm: unknown vector"
  where
    [x, y, z] = toList v

invm :: M -> M
invm = compute . A.map negate

turnw :: Turn -> Walker -> Walker
turnw t (Walker d o) = Walker d' o
  where
    fw = getpm o
    bw = invm fw
    trn TRight = turnr
    trn TLeft = turnl
    d' = compute $ (bw !><! trn t !><! fw) !>< d

w :: Walker
w = Walker (froml [1, 0, 0]) (froml [0, 0, 1])

data Walker = Walker
  { direction :: V,
    orientation :: V
  }
  deriving (Show, Eq)

-- -- Direction in one dimension.
-- data D1
--   = -- Look into positive direction (indices are increasing).
--     Pos
--   | -- Look into negative direction (indices are decreasing).
--     Neg
--   deriving (Show, Eq, Ord)

-- data A3 = Row | Col | Dep
--   deriving (Show, Eq, Ord)

-- -- Direction in three dimensions.
-- data D3 = D3 {d1 :: D1, ax :: A3}
--   deriving (Show, Eq, Ord)

-- -- Turn in the given direction from a face pointing into a 'D3' around to
-- -- another face pointing into a new 'D3'.
-- turnFace :: Direction -> D3 -> D3
-- -- Front.
-- turnFace DRight (D3 Neg Dep) = D3 Pos Col
-- turnFace DUp (D3 Neg Dep) = D3 Pos Row
-- -- Right side.
-- turnFace DRight (D3 Pos Col) = D3 Pos Dep
-- turnFace DUp (D3 Pos Col) = D3 Pos Row
-- -- Top.
-- turnFace DRight (D3 Pos Row) = D3 Pos Col
-- turnFace DUp (D3 Pos Row) = D3 Pos Dep
-- -- Left side.
-- turnFace DRight (D3 Neg Col) = D3 Neg Dep
-- turnFace DUp (D3 Neg Col) = D3 Pos Dep
-- -- Back.
-- turnFace DRight (D3 Neg Row) = D3 Neg Dep
-- turnFace DUp (D3 Neg Row) = D3 Pos Col
-- --
-- turnFace DRight (D3 Pos Dep) = D3 Neg Dep
-- turnFace DUp (D3 Pos Dep) = D3 Pos Col

-- type Face = Field

-- type Faces = M.Map D3 Face

main :: IO ()
main = do
  d <- BS.readFile "inputs/input22.txt"
  -- Part 1.
  let (xs, is) = either error id $ parseOnly pInput d
      x0 = findStart xs
  print $ uncurry grade $ move xs is DRight x0
  -- Part 2.
  undefined
