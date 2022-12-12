-- |
-- Module      :  Main
-- Description :  Day 3; Rucksack Reorganization
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sat Dec  3 07:36:42 2022.
--
-- See https://adventofcode.com/2022/day/3.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isLower, isUpper, ord)
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
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = error "priority: not an alpha character"

findShared :: T.Text -> T.Text -> T.Text -> Char
findShared a b = fromJust . T.find (\x -> x `S.member` sa && x `S.member` sb)
  where
    sa = assembleSet a
    sb = assembleSet b

findAllShared :: [T.Text] -> String
findAllShared [] = ""
findAllShared (x : y : z : zs) = findShared x y z : findAllShared zs
findAllShared _ = error "findAllShared: not multiple of three"

main :: IO ()
main = do
  t <- T.readFile "inputs/input03.txt"
  -- Part 1.
  let xs = either error id $ parseOnly pInput t
  print $ sum $ map (priority . findDuplicate) xs
  -- Part 2.
  print $ sum $ map priority $ findAllShared xs
