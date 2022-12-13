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

import Aoc.List (chop)
import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    decimal,
    endOfInput,
    parseOnly,
    sepBy1',
    signed,
    skipSpace,
    string,
  )
import qualified Data.ByteString.Char8 as BS
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

data Elem = S Int | N NList
  deriving (Eq)

infixr 5 :.

data NList = Elem :. NList | Nil
  deriving (Eq)

-- The 'Show' instances are not necessary but are useful for debugging.

instance Show Elem where
  showsPrec d (S x) = showsPrec d x
  showsPrec d (N xs) = showsPrec d xs

instance Show NList where
  -- Show list with brackets.
  showsPrec d xs = showChar '[' . showsPrec' d xs . showChar ']'

-- Show list without brackets.
showsPrec' :: Int -> NList -> ShowS
showsPrec' d (x :. Nil) = showsPrec d x
showsPrec' d (x :. xs) = showsPrec d x . showChar ',' . showsPrec' d xs
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
pNext = (char ',' *> pElems) <|> pEnd

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

-- Part 1.

grade :: Int -> Ordering -> Int
grade n LT = n
grade _ _ = 0

-- Part 2.

driver1 :: NList
driver1 = N (S 2 :. Nil) :. Nil

driver2 :: NList
driver2 = N (S 6 :. Nil) :. Nil

main :: IO ()
main = do
  b <- BS.readFile "inputs/input13.txt"
  let xs = either error id $ parseOnly pInput b
  -- Part 1.
  let ps = hInput xs
      os = map (uncurry compare) ps
  print $ sum $ zipWith grade [1 ..] os
  -- Part 2.
  let xs' = sort $ driver1 : driver2 : xs
      i1 = fromJust $ elemIndex driver1 xs'
      i2 = fromJust $ elemIndex driver2 xs'
  print $ product $ map (+ 1) [i1, i2]
