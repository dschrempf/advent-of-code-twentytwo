-- |
-- Module      :  Main
-- Description :  Day 23; Unstable Diffusion
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/23.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Massiv.Array
  ( Array,
    B,
    Comp (..),
    Ix2,
  )
import qualified Data.Massiv.Array as A

data Tile = Elf | Ground
  deriving (Show, Eq)

toChar :: Tile -> Char
toChar Elf = '#'
toChar Ground = '.'

pTile :: Parser Tile
pTile = Ground <$ char '.' <|> Elf <$ char '#'

isElf :: Tile -> Bool
isElf Elf = True
isElf Ground = False
{-# INLINE isElf #-}

isGround :: Tile -> Bool
isGround Elf = False
isGround Ground = True
{-# INLINE isGround #-}

type Field = Array B Ix2 Tile

pField :: Parser Field
pField = A.fromLists' Seq <$> some pTile `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

showField :: Field -> String
showField = unlines . map (map toChar) . A.toLists

main :: IO ()
main = do
  d <- BS.readFile "inputs/input23-sample.txt"
  let f = either error id $ parseOnly pField d
  putStrLn $ showField f
