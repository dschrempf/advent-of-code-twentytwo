{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 13; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/13.
module Main
  ( main,
  )
where

import Aoc.List
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

infixr 5 :.

data Elem = S Int | N NList
  deriving (Eq)

instance Show Elem where
  showsPrec d (S x) = showsPrec d x
  showsPrec d (N xs) = showsPrec d xs

data NList = Elem :. NList | Nil
  deriving (Eq)

instance Show NList where
  -- Show list with brackets.
  showsPrec d xs = showChar '[' . showsPrec' d xs . showChar ']'

-- Show list without brackets.
showsPrec' :: Int -> NList -> ShowS
showsPrec' d (S i :. Nil) = showsPrec d i
showsPrec' d (N xs :. Nil) = showsPrec d xs
showsPrec' d (S i :. xs) = showsPrec d i . showString ", " . showsPrec' d xs
showsPrec' d (N ys :. xs) = showsPrec d ys . showString ", " . showsPrec' d xs
showsPrec' _ Nil = id

instance Ord NList where
  compare Nil Nil = EQ
  compare _ Nil = GT
  compare Nil _ = LT
  compare (S x :. xs) (S y :. ys) = case compare x y of
    LT -> LT
    GT -> GT
    EQ -> compare xs ys
  compare (S x :. xs) ys = compare (N (S x :. Nil) :. xs) ys
  compare xs (S y :. ys) = compare xs (N (S y :. Nil) :. ys)
  compare (N xs :. xss) (N ys :. yss) = case compare xs ys of
    LT -> LT
    GT -> GT
    EQ -> compare xss yss

-- Parse 'NList' with starting bracket.
pNList :: Parser NList
pNList = pEmpty <|> pNonEmpty

pEmpty :: Parser NList
pEmpty = Nil <$ string "[]"

pNonEmpty :: Parser NList
pNonEmpty = char '[' *> pElems

-- Parse 'NList' without starting bracket.
pElems :: Parser NList
pElems = do
  e <- pElemS <|> pElemN
  n <- pNext
  pure $ e :. n

pNext :: Parser NList
pNext = (string ", " *> pElems) <|> pEnd

pEnd :: Parser NList
pEnd = Nil <$ char ']'

-- Parse simple list without starting bracket: (Int :. NList).
pElemS :: Parser Elem
pElemS = S <$> signed decimal

-- Parse nested list without starting bracket (NList ::. NList).
pElemN :: Parser Elem
pElemN = N <$> pNList

-- -- Property testing of (parse . show ~ id) would be adequate here.
-- pParseShow :: NList -> Bool
-- pParseShow xs = either error id (parseOnly pNList (BS.pack $ show xs)) == xs

-- res :: [Bool]
-- res = map pParseShow [nested0, nested1, nested2, nested3, simple0, simple1, nil]

-- -- nested0 = [[1, 2, [1, 2], [2]]]
-- nested0 = S 1 :. S 2 :. N (N (S 1 :. S 2 :. Nil) :. N (S 2 :. Nil) :. Nil) :. Nil

-- -- nested1 = [1, 2, [1, 2], [2]]
-- nested1 = S 1 :. S 2 :. N (S 1 :. S 2 :. Nil) :. N (S 2 :. Nil) :. Nil

-- -- nested2 = [[1]]
-- nested2 = N (S 1 :. Nil) :. Nil

-- -- nested3 = [[]]
-- nested3 = N Nil :. Nil

-- -- simple0 = [1, 2]
-- simple0 = S 1 :. S 2 :. Nil

-- -- simple1 = [1]
-- simple1 = S 1 :. Nil

-- -- nil = []
-- nil = Nil

pInput :: Parser [NList]
pInput = pNList `sepBy1'` skipSpace <* skipSpace <* endOfInput

hInput :: [NList] -> [(NList, NList)]
hInput = map pair . chop 2
  where
    pair [x, y] = (x, y)
    pair _ = error "pair: no pair"

main :: IO ()
main = do
  b <- BS.readFile "inputs/input13-sample.txt"
  let ps = hInput $ either error id $ parseOnly pInput b
  undefined
