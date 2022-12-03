-- |
-- Module      :  Day03
-- Description :  Rucksacks
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sat Dec  3 07:36:42 2022.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isLower, isUpper)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

assembleSet :: T.Text -> S.Set Char
assembleSet = T.foldl' (flip S.insert) S.empty

findDuplicate :: T.Text -> Char
findDuplicate x = fromJust $ T.find (`S.member` s) x2
  where
    l = T.length x
    l2 = l `div` 2
    (x1, x2) = T.splitAt l2 x
    s = assembleSet x1

pRucksack :: Parser T.Text
pRucksack = takeWhile1 (not . isEndOfLine)

pInput :: Parser [T.Text]
pInput = pRucksack `sepBy1` endOfLine <* optional endOfLine <* endOfInput

priority :: Char -> Int
priority c
  | isLower c = fromEnum c - fromEnum 'a' + 1
  | isUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = error "priority: not an alpha character"

main :: IO ()
main = do
  t <- T.readFile "inputs/input03.txt"
  let xs = either error id $ parseOnly pInput t
      ds = map findDuplicate xs
  print $ sum $ map priority ds
