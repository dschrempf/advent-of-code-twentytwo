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
import qualified Data.Map.Strict as M
import Data.Massiv.Array
  ( Array,
    B,
    Comp (..),
    Ix2 (..),
    Ix3,
    IxN (..),
    Sz (..),
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
import Data.Maybe (fromJust)

data Cell = Tile | Wall | Void
  deriving (Show, Eq)

pCell :: Parser Cell
pCell = tile <|> wall <|> void
  where
    tile = Tile <$ char '.'
    wall = Wall <$ char '#'
    void = Void <$ char ' '

type F2 = Array B Ix2 Cell

pLine :: Parser [Cell]
pLine = some pCell

-- Also, add a 'Void' frame; see 'wrap'.
toField :: [[Cell]] -> F2
toField xs = fromLists' Seq $ frameLine : map elongate xs ++ [frameLine]
  where
    maxLength = maximum $ map length xs
    elongate ys = Void : ys ++ replicate (maxLength + 1 - length ys) Void
    frameLine = replicate (maxLength + 2) Void

pField :: Parser F2
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

pInput :: Parser (F2, Instructions)
pInput = do
  xs <- pField
  _ <- count 2 endOfLine
  is <- pInstructions
  _ <- optional endOfLine
  _ <- endOfInput
  pure (xs, is)

-- Part 1.

data D2 = DLeft | DDown | DRight | DUp
  deriving (Show, Eq, Bounded, Enum)

type P2 = Ix2

-- Walker in two dimensions.
data W2 = W2
  { d2Dir :: D2,
    d2Pos :: P2
  }

forwards :: D2 -> P2 -> P2
forwards d (Ix2 i j) = case d of
  DLeft -> i :. pred j
  DDown -> succ i :. j
  DRight -> i :. succ j
  DUp -> pred i :. j

opposite :: D2 -> D2
opposite DLeft = DRight
opposite DRight = DLeft
opposite DUp = DDown
opposite DDown = DUp

backwards :: D2 -> P2 -> P2
backwards = forwards . opposite

-- Dragons.
--
-- Move backwards until finding 'Void'. We can do this since we framed the board
-- with 'Void'.
wrap :: F2 -> D2 -> P2 -> P2
wrap field direction position = go position field direction position
  where
    go :: P2 -> F2 -> D2 -> P2 -> P2
    go p0 xs d p = case x' of
      -- Need to check if this is a wall. If so, provide the original position.
      Void -> if (xs ! p) == Wall then p0 else p
      _ -> go p0 xs d p'
      where
        p' = backwards d p
        x' = xs ! p'

moveOne :: F2 -> D2 -> P2 -> P2
moveOne xs d p = case x' of
  Wall -> p
  Tile -> p'
  Void -> wrap xs d p
  where
    p' = forwards d p
    x' = xs ! p'

turn :: Turn -> D2 -> D2
turn TRight DLeft = DUp
turn TRight d = pred d
turn TLeft DUp = DLeft
turn TLeft d = succ d

move :: F2 -> Instructions -> D2 -> P2 -> (D2, P2)
move xs (NCons n is') d p = move xs is' d $ nTimesStrict n (moveOne xs d) p
move xs (TCons t is') d p = move xs is' (turn t d) p
move _ Nil d p = (d, p)

findStart :: F2 -> P2
findStart = fromJust . findIndex (== Tile)

grade :: D2 -> P2 -> Int
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
type P3 = Ix3

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

data W3 = W3
  { _direction :: V,
    _orientation :: V,
    _position :: P3
  }
  deriving (Show, Eq)

turnw :: Turn -> W3 -> W3
turnw t (W3 d o p) = W3 d' o p
  where
    d' = compute $ getTurn t o !>< d

cross :: V -> V -> V
cross a b = froml [a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1]
  where
    [a1, a2, a3] = toList a
    [b1, b2, b3] = toList b

-- Flip walker over an edge.
flipw :: W3 -> W3
flipw (W3 d o p) = W3 (t `g` d) (t `g` o) p
  where
    f = cross d o
    t = getTurn TLeft f
    g m v = compute $ m !>< v

w0 :: W3
w0 = W3 (froml [1, 0, 0]) (froml [0, 0, 1]) (1 :> 1 :. 1)

movew :: W3 -> W3
movew w@(W3 d o p)
  -- We are at an edge, flip the walker and move one more field to get back onto
  -- the face of the cube.
  | isEdge p' = movew $ flipw (W3 d o p')
  | otherwise = W3 d o p'
  where
    [dx, dy, dz] = toList d
    (Ix3 x y z) = p
    p' = Ix3 (x + dx) (y + dy) (z + dz)
    isEdge (Ix3 a b c) = 0 `elem` [a, b, c]

-- Position map.
type PMap = M.Map P3 P2

data Positions = Positions
  { d2dir :: D2,
    d2pos :: P2,
    d3wlk :: W3
  }

data Fields = Fields
  { d2fld :: F2,
    pmap :: PMap
  }

data State = State
  { positions :: Positions,
    fields :: Fields
  }

move2d :: F2 -> D2 -> P2 -> Maybe P2
move2d f2 d p = case f2 ! p' of
  Void -> Nothing
  _ -> Just p'
  where
    p' = forwards d p

moveps :: F2 -> Positions -> Maybe Positions
moveps xs (Positions d p w) = do
  p' <- move2d xs d p
  pure $ Positions d p' $ movew w

fillPos :: Positions -> Fields -> Fields
fillPos (Positions _ p2 (W3 _ _ p3)) (Fields f2 pm) = Fields f2 (M.insert p3 p2 pm)

-- Return 'Nothing' if we are at the end of the field.
moveDown :: State -> Maybe State
moveDown (State ps fs) = undefined

fillFields' :: State -> State
fillFields' (State ps fs) = undefined
  where
    fs' = fillPos ps fs
    ps' = moveps (d2fld fs) ps

fillFields :: F2 -> P2 -> W3 -> Fields
fillFields f2 p0 w0 = fields $ fillFields' (State ps fs)
  where
    ps = Positions DRight p0 w0
    fs = Fields f2 M.empty

main :: IO ()
main = do
  d <- BS.readFile "inputs/input22.txt"
  -- Part 1.
  let (xs, is) = either error id $ parseOnly pInput d
      x0 = findStart xs
  print $ uncurry grade $ move xs is DRight x0
  -- Part 2.
  undefined
