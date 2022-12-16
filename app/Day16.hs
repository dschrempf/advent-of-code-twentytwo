{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 16; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/16.
module Main
  ( main,
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

data Valve = Valve
  { name :: String,
    flowRate :: Int,
    tunnels :: [String]
  }
  deriving (Show)

pValveName :: Parser String
pValveName = count 2 letter_ascii <?> "vname"

pTunnelString :: Parser ()
pTunnelString =
  void $
    string "; tunnels lead to valves "
      <|> string "; tunnel leads to valve "

pValve :: Parser Valve
pValve = do
  _ <- string "Valve "
  n <- pValveName
  _ <- string " has flow rate="
  r <- decimal
  _ <- pTunnelString
  ts <- pValveName `sepBy1'` string ", "
  pure $ Valve n r ts

pInput :: Parser [Valve]
pInput = pValve `sepBy1` endOfLine <* optional endOfLine <* endOfInput <?> "input"

main :: IO ()
main = do
  d <- BS.readFile "inputs/input16-sample.txt"
  let vs = either error id $ parseOnly pInput d
  print vs
