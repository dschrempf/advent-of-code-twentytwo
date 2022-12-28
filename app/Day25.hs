-- |
-- Module      :  Main
-- Description :  Day 25; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/25.
module Main
  ( main,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    choice,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
  )
import qualified Data.ByteString.Char8 as BS

data SnafuDigit = MinusTwo | MinusOne | Zero | PlusOne | PlusTwo
  deriving (Show, Eq, Bounded, Enum)

toChar :: SnafuDigit -> Char
toChar MinusTwo = '='
toChar MinusOne = '-'
toChar Zero = '0'
toChar PlusOne = '1'
toChar PlusTwo = '2'

toInt :: SnafuDigit -> Int
toInt MinusTwo = -2
toInt MinusOne = -1
toInt Zero = 0
toInt PlusOne = 1
toInt PlusTwo = 2

parseSnafuDigit :: Parser SnafuDigit
parseSnafuDigit = choice $ map f [MinusTwo ..]
  where
    f c = c <$ char (toChar c)

type Snafu = [SnafuDigit]

parseSnafu :: Parser Snafu
parseSnafu = some parseSnafuDigit

parseInput :: Parser [Snafu]
parseInput = parseSnafu `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

snafuToInt :: Snafu -> Int
snafuToInt = go (0 :: Int) . reverse
  where
    go :: Int -> [SnafuDigit] -> Int
    go _ [] = 0
    go n (x : xs) = 5 ^ n * toInt x + go (succ n) xs

intToSnafu' :: Int -> Snafu
intToSnafu' 0 = []
intToSnafu' x = case r of
  0 -> Zero : intToSnafu' x'
  1 -> PlusOne : intToSnafu' x'
  2 -> PlusTwo : intToSnafu' x'
  3 -> MinusTwo : intToSnafu' ((x + 2) `div` 5)
  4 -> MinusOne : intToSnafu' ((x + 1) `div` 5)
  _ -> error "intToSnafu': bug; modulo five out of range"
  where
    (x', r) = x `divMod` 5

intToSnafu :: Int -> Snafu
intToSnafu = reverse . intToSnafu'

main :: IO ()
main = do
  d <- BS.readFile "inputs/input25.txt"
  let xs = either error id $ parseOnly parseInput d
      s = sum $ map snafuToInt xs
  putStrLn $ map toChar $ intToSnafu s
