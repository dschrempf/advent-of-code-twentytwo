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

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

infixr 5 :.

infixr 4 ::.

data RList = Int :. RList | RList ::. RList | Nil
  deriving (Eq)

instance Show RList where
  -- Show list with brackets.
  showsPrec d xs = showChar '[' . showsPrec' d xs . showChar ']'

-- Show list without brackets.
showsPrec' :: Int -> RList -> ShowS
showsPrec' d (i :. Nil) = showsPrec d i
showsPrec' d (lsl ::. Nil) = showsPrec d lsl
showsPrec' d (i :. ls) = showsPrec d i . showString ", " . showsPrec' d ls
showsPrec' d (lsl ::. lsr) = showsPrec d lsl . showString ", " . showsPrec d lsr
showsPrec' _ _ = id

-- Parse 'RList' with starting bracket.
pRList :: Parser RList
pRList = pEmpty <|> pNonEmpty

pEmpty :: Parser RList
pEmpty = Nil <$ string "[]"

pNonEmpty :: Parser RList
pNonEmpty = char '[' *> pElem

-- Parse 'RList' without starting bracket.
pElem :: Parser RList
pElem = pRElemS <|> pRElemN

pNext :: Parser RList
pNext = (string ", " *> pElem) <|> pEnd

pEnd :: Parser RList
pEnd = Nil <$ char ']'

-- Parse simple list without starting bracket: (Int :. RList).
pRElemS :: Parser RList
pRElemS = do
  i <- signed decimal
  l <- pNext
  pure $ i :. l

-- Parse nested list without starting bracket (RList ::. RList).
pRElemN :: Parser RList
pRElemN = do
  xs <- pRList
  ys <- pNext
  case ys of
    Nil -> pure xs
    _ -> pure $ xs ::. ys

-- nested = [[1, 2, [1, 2], [2]]]
nested = 1 :. 2 :. (1 :. 2 :. Nil ::. 2 :. Nil) ::. Nil

-- nesteda = [1, 2, [1, 2], [2]]
nesteda = 1 :. 2 :. (1 :. 2 :. Nil ::. 2 :. Nil)

-- nested1 = [[1]]
nested1 = 1 :. Nil ::. Nil

-- nested2 = [[]]
nested2 = Nil ::. Nil

-- simple = [1, 2]
simple = 1 :. 2 :. Nil

-- simple1 = [1]
simple1 = 1 :. Nil

-- nil = []
nil = Nil

main :: IO ()
main = undefined
