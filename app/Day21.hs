{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 21; Monkey Math
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
import Data.Tree (Tree (..))

type Name = BS.ByteString

data Op = Ad | Su | Mu | Di

getOp :: Op -> (Int -> Int -> Int)
getOp Ad = (+)
getOp Su = (-)
getOp Mu = (*)
getOp Di = div

data MonkeyI
  = MonkeyO
      { _oarg1 :: Name,
        _op :: Op,
        _oarg2 :: Name
      }
  | MonkeyE
      { _earg1 :: Name,
        _earg2 :: Name
      }
  | MonkeyL Int
  | MonkeyU

-- For debugging.
instance Show MonkeyI where
  show (MonkeyO a1 _ a2) = show $ a1 <> " <> " <> a2
  show (MonkeyL n) = show n
  show (MonkeyE a1 a2) = show $ a1 <> " == " <> a2
  show MonkeyU = "?"

pName :: Parser Name
pName = takeWhile1 isAlpha_ascii

pOp :: Parser Op
pOp = choice [pAd, pSu, pMu, pDi]
  where
    f o c = o <$ char c
    pAd = f Ad '+'
    pSu = f Su '-'
    pMu = f Mu '*'
    pDi = f Di '/'

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
  pure (n, i)

pInput :: Parser [(Name, MonkeyI)]
pInput = pMonkey `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

type Monkeys = M.Map Name MonkeyI

data MonkeyN = O Op | E | L Int | U

-- For debugging.
instance Show MonkeyN where
  show (O _) = "<>"
  show E = "=="
  show (L n) = show n
  show U = "?"

type MonkeyT = Tree MonkeyN

bTree :: Monkeys -> MonkeyT
bTree ms = go r
  where
    r = ms M.! "root"
    go :: MonkeyI -> MonkeyT
    go (MonkeyO x o y) = Node (O o) [go (ms M.! x), go (ms M.! y)]
    go (MonkeyE x y) = Node E [go (ms M.! x), go (ms M.! y)]
    go (MonkeyL n) = Node (L n) []
    go MonkeyU = Node U []

-- Part 1.

cTree :: MonkeyT -> Either String Int
cTree (Node (L n) []) = Right n
cTree (Node (O o) [x, y]) = getOp o <$> cTree x <*> cTree y
cTree (Node (L _) _) = Left "cTree: literal at internal node"
cTree (Node U _) = Left "cTree: unknown literal"
cTree (Node (O _) _) = Left "cTree: operation with wrong number of arguments"
cTree (Node E _) = Left "cTree: cannot handle equality"

-- Part 2.

bTree2 :: Monkeys -> MonkeyT
bTree2 ms = bTree $ M.adjust toEq "root" $ M.insert "humn" MonkeyU ms
  where
    toEq (MonkeyO l _ r) = MonkeyE l r
    toEq _ = error "bTree2: unexpected root label"

-- Get inverse; calculate left operand.
getInvL ::
  Op ->
  ( -- Top operand.
    Int ->
    -- Right operand.
    Int ->
    -- Left operand.
    Int
  )
getInvL Ad = (-)
getInvL Mu = div
getInvL Su = (+)
getInvL Di = (*)

-- Get inverse; calculate right operand.
getInvR ::
  Op ->
  ( -- Top operand.
    Int ->
    -- Left operand.
    Int ->
    -- Right operand.
    Int
  )
getInvR Ad = (-)
getInvR Mu = div
getInvR Su = \t l -> negate t + l
getInvR Di = flip div

solveTreeWith :: Int -> MonkeyT -> Either String Int
solveTreeWith n (Node U []) = Right n
solveTreeWith n (Node (O o) [x, y]) = case (cTree x, cTree y) of
  (Right l, Left _) -> solveTreeWith (getInvR o n l) y
  (Left _, Right r) -> solveTreeWith (getInvL o n r) x
  (Left _, Left _) -> Left "solveTreeWith: both subtrees are unknown"
  (Right _, Right _) -> Left "solveTreeWith: both subtrees are known"
solveTreeWith _ _ = Left "solveTreeWith: unhandled operation"

solveTree :: MonkeyT -> Either String Int
solveTree (Node E [x, y]) = case (cTree x, cTree y) of
  (Right l, Left _) -> solveTreeWith l y
  (Left _, Right r) -> solveTreeWith r x
  (Left _, Left _) -> Left "solveTree: both subtrees are unknown"
  (Right _, Right _) -> Left "solveTree: both subtrees are known"
solveTree (Node E _) = Left "solveTree: equality operation with wrong number of arguments"
solveTree _ = Left "solveTree: no equality operation at root"

main :: IO ()
main = do
  d <- BS.readFile "inputs/input21.txt"
  let ms = either error id $ parseOnly pInput d
      mm = M.fromList ms
      mt = bTree mm
  -- Part 1.
  print $ cTree mt
  -- Part 2.
  let t2 = bTree2 mm
  print $ solveTree t2
