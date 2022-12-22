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

import Aoc.Function (nTimesLazy, nTimesStrict)
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
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Massiv.Array
  ( Array,
    B (..),
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

-- Walker on two-dimensional field.
data W2 = W2
  { w2Direction :: D2,
    w2Position :: P2
  }
  deriving (Show, Eq)

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
wrap f2 dr ps = go ps f2 dr ps
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

-- After thinking for a while, and since I am not going for the leaderboard, I
-- decided to implement rotations and the lot using proper matrix algebra.

type V = Vector U Int

type M = Array U Ix2 Int

-- Position.
type P3 = Ix3

fromL :: [Int] -> V
fromL = fromList Seq

fromLs :: [[Int]] -> M
fromLs = fromLists' Seq

pos :: Turn -> Int
pos TRight = 1
pos TLeft = -1

neg :: Turn -> Int
neg TRight = -1
neg TLeft = 1

rotX :: Turn -> M
rotX t =
  fromLs
    [ [1, 0, 0],
      [0, 0, neg t],
      [0, pos t, 0]
    ]

rotY :: Turn -> M
rotY t =
  fromLs
    [ [0, 0, pos t],
      [0, 1, 0],
      [neg t, 0, 0]
    ]

rotZ :: Turn -> M
rotZ t =
  fromLs
    [ [0, neg t, 0],
      [pos t, 0, 0],
      [0, 0, 1]
    ]

flipT :: Turn -> Turn
flipT TRight = TLeft
flipT TLeft = TRight

-- For a given turn and an orientation, calculate the rotation matrix.
getTurn :: Turn -> V -> M
getTurn t o = case toList o of
  [1, 0, 0] -> rotX t
  [-1, 0, 0] -> rotX $ flipT t
  [0, 1, 0] -> rotY t
  [0, -1, 0] -> rotY $ flipT t
  [0, 0, 1] -> rotZ t
  [0, 0, -1] -> rotZ $ flipT t
  _ -> error $ "getTurn: unknown orientation: " ++ show o

-- A walker in three-dimensional space.
data W3 = W3
  { w3Direction :: V,
    w3Orientation :: V,
    w3Position :: P3,
    w3CubeSize :: Int
  }
  deriving (Show, Eq)

turnW3 :: Turn -> W3 -> W3
turnW3 t (W3 d o p n) = W3 d' o p n
  where
    d' = compute $ getTurn t o !>< d

cross :: V -> V -> V
cross a b = fromL [a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1]
  where
    [a1, a2, a3] = toList a
    [b1, b2, b3] = toList b

-- Flip walker over an edge.
flipW3 :: W3 -> W3
flipW3 (W3 d o p n) = W3 (t `g` d) (t `g` o) p n
  where
    f = cross d o
    t = getTurn TRight f
    g m v = compute $ m !>< v

moveW3 :: W3 -> W3
moveW3 (W3 d o p n)
  -- We are at an edge, flip the walker and move one more field to get back onto
  -- the face of the cube.
  | isEdge3D p' = moveW3 $ flipW3 (W3 d o p' n)
  | otherwise = W3 d o p' n
  where
    [dx, dy, dz] = toList d
    (Ix3 x y z) = p
    p' = Ix3 (x + dx) (y + dy) (z + dz)
    isEdge1D a = a == 0 || a == 51
    isEdge3D (Ix3 a b c) =
      let l = length $ filter (== True) $ map isEdge1D [a, b, c]
       in l >= 2

-- Position map from positions in three dimensional cube to positions on the
-- two-dimensional field.
type PMap = M.Map P3 P2

data Walkers = Walkers
  { d2Walker :: W2,
    d3Walker :: W3
  }
  deriving (Show, Eq)

data Fields = Fields
  { d2Field :: F2,
    pMap :: PMap
  }
  deriving (Show, Eq)

data State = State
  { positions :: Walkers,
    fields :: Fields
  }
  deriving (Show, Eq)

turnW2 :: Turn -> W2 -> W2
turnW2 t (W2 d p) = W2 (turn t d) p

moveW2 :: F2 -> W2 -> Maybe W2
moveW2 xs (W2 d p) = case xs ! p' of
  Void -> Nothing
  _ -> Just $ W2 d p'
  where
    p' = forwards d p

moveWsUntilVoid :: F2 -> Walkers -> Walkers
moveWsUntilVoid xs ws = case moveWsForward xs ws of
  Nothing -> ws
  Just ws' -> moveWsUntilVoid xs ws'

turnWsAround :: Walkers -> Walkers
turnWsAround (Walkers w2 w3) = Walkers w2o w3o
  where
    t = TRight
    w2o = nTimesLazy 2 (turnW2 t) w2
    w3o = nTimesLazy 2 (turnW3 t) w3

-- Return 'Nothing' if we are at the end of the field.
moveWsDown :: F2 -> Walkers -> Maybe Walkers
moveWsDown xs (Walkers (W2 d p) w3) = do
  w2' <- turnW2 (flipT t) <$> moveW2 xs (W2 DDown p)
  let w3' = turnW3 (flipT t) $ moveW3 $ turnW3 t w3
  pure $ turnWsAround $ moveWsUntilVoid xs $ Walkers w2' w3'
  where
    t = case d of
      DRight -> TRight
      DLeft -> TLeft
      _ -> error "moveWsDown: wrong direction"

moveWsForward :: F2 -> Walkers -> Maybe Walkers
moveWsForward xs (Walkers w2 w3) = do
  w2' <- moveW2 xs w2
  let w3' = moveW3 w3
  pure $ Walkers w2' w3'

moveWsOnF2 :: F2 -> Walkers -> Maybe Walkers
moveWsOnF2 xs ws =
  moveWsForward xs ws
    <|> moveWsDown xs ws
    <|> moveWsDown xs (moveWsUntilVoid xs $ turnWsAround ws)

fillPos :: Walkers -> Fields -> Fields
fillPos (Walkers (W2 _ p2) (W3 _ _ p3 _)) (Fields xs pm) = Fields xs (M.insert p3 p2 pm)

fillFields' :: State -> State
fillFields' (State ws fs) = case mws' of
  Nothing -> State ws fs'
  Just ws' -> fillFields' (State ws' fs')
  where
    fs' = fillPos ws fs
    mws' = moveWsOnF2 (d2Field fs) ws

-- Simultaneously walk along the field and the cube and log positions.
fillPMap :: F2 -> W2 -> W3 -> PMap
fillPMap xs w2 w3 = pMap $ fields $ fillFields' (State ps fs)
  where
    ps = Walkers w2 w3
    fs = Fields xs M.empty

move3dOne :: Fields -> W3 -> W3
move3dOne (Fields xs m) w = case x' of
  Void -> error "move3dOne: walked into 'Void'"
  Tile -> w'
  Wall -> w
  where
    w' = moveW3 w
    x' = xs ! (m M.! w3Position w')

move3d :: Fields -> Instructions -> W3 -> W3
move3d f (NCons n is') w = move3d f is' $ nTimesLazy n (move3dOne f) w
move3d f (TCons t is') w = move3d f is' $ turnW3 t w
move3d _ Nil w = w

getDirection :: PMap -> W3 -> D2
getDirection m w
  | r + 1 == r' = DDown
  | r - 1 == r' = DUp
  | c + 1 == c' = DRight
  | c - 1 == c' = DLeft
  | otherwise = error "getDirection: could not find direction"
  where
    p3 = w3Position w
    p3' = w3Position $ moveW3 w
    (Ix2 r c) = m M.! p3
    (Ix2 r' c') = m M.! p3'

main :: IO ()
main = do
  d <- BS.readFile "inputs/input22.txt"
  -- Part 1.
  let (xs, is) = either error id $ parseOnly pInput d
      x0 = findStart xs
  print $ uncurry grade $ move xs is DRight x0
  -- Part 2.
  let -- Walker on two-dimensional field.
      w2 = W2 DRight x0
      (Sz1 nCells) = A.size $ A.computeAs B $ A.sfilter (/= Void) xs
      -- Stupid way to calculate cube size (no need to take square root in
      -- discrete decimal domain).
      cubeSz = fromJust $ find (\n -> n * n * 6 == nCells) [1 ..]
      -- Walker on three-dimensional cube.
      w3 = W3 (fromL [1, 0, 0]) (fromL [0, 0, 1]) (1 :> 1 :. 0) cubeSz
      -- Calculate the position map.
      pm = fillPMap xs w2 w3
  -- -- There should be no index pointing to 'Void'.
  -- let cm = M.map (xs !) pm
  -- print $ M.filter (== Void) cm
  -- -- The length of the map should be the number of non-'Void' cells.
  -- print $ M.size $ M.filter (/= Void) cm
  -- print $ size $ computeAs B $ sfilter (/= Void) xs
  let -- End position of walker on three-dimensional cube.
      w3' = move3d (Fields xs pm) is w3
      -- Convert the direction and position back to the two-dimensional field.
      dr = getDirection pm w3'
      p2' = pm M.! w3Position w3'
  print $ grade dr p2'
