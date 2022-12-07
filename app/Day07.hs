{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 7; No Space Left On Device
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Dec  6 10:06:20 2022.
--
-- See https://adventofcode.com/2022/day/7.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.Text.Lazy
import Data.Char
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

data File = File
  { fName :: TS.Text,
    fSize :: Integer
  }

data DirectoryTree = Directory
  { dName :: TS.Text,
    files :: [File],
    subdirectories :: [DirectoryTree]
  }

data Command = CdUp | CdDir TS.Text | Ls
  deriving (Show, Eq)

pAtom :: Parser TS.Text
pAtom = takeWhile1 (not . isSpace)

pCommand :: Parser Command
pCommand = string "$ " *> (pCdUp <|> pCdDir <|> pLs) <* optional endOfLine
  where
    pCdUp = CdUp <$ string "cd .."
    pCdDir = CdDir <$> (string "cd " *> pAtom)
    pLs = Ls <$ string "ls"

pFile :: Parser File
pFile = do
  d <- decimal
  _ <- char ' '
  n <- pAtom
  _ <- optional endOfLine
  pure $ File n d

pDirectory :: Parser DirectoryTree
pDirectory = string "dir " *> pDir <* optional endOfLine
  where
    toDir n = Directory n [] []
    pDir = toDir <$> pAtom

pInput :: Parser DirectoryTree
pInput = undefined

main :: IO ()
main = do
  b <- TL.readFile "inputs/input07-sample.txt"
  print ""
