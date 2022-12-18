-- |
-- Module      :  Main
-- Description :  Day 17; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/17.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

data Jet = L | R
  deriving (Show, Eq)

pJet :: Parser Jet
pJet = (L <$ char '<') <|> (R <$ char '>')

type JetPattern = [Jet]

pInput :: Parser JetPattern
pInput = some pJet <* optional endOfLine <* endOfInput

main :: IO ()
main = do
  d <- BS.readFile "inputs/input17-sample.txt"
  let jp = either error id $ parseOnly pInput d
  print jp
