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
  deriving (Show)

data DirTree = DirNode
  { dName :: TS.Text,
    files :: [File],
    subdirectories :: [DirTree]
  }
  deriving (Show)

data CdCmd = CdRoot | CdUp | CdDir TS.Text
  deriving (Show, Eq)

data Cmd = Cd CdCmd | Ls
  deriving (Show, Eq)

pAtom :: Parser TS.Text
pAtom = takeWhile1 (not . isSpace)

pCdCmd :: Parser CdCmd
pCdCmd = string "cd " *> (pCdRoot <|> pCdUp <|> pCdDir)
  where
    pCdRoot = CdRoot <$ char '/'
    pCdUp = CdUp <$ string ".."
    pCdDir = CdDir <$> pAtom

pCmd :: Parser Cmd
pCmd = string "$ " *> (pCd <|> pLs)
  where
    pCd = Cd <$> pCdCmd
    pLs = Ls <$ string "ls"

pFile :: Parser File
pFile = do
  d <- decimal
  _ <- char ' '
  n <- pAtom
  pure $ File n d

newtype Dir = Dir {getDirName :: TS.Text}

pDir :: Parser Dir
pDir = string "dir " *> (Dir <$> pAtom)

data Line = LCmd Cmd | LFile File | LDir Dir

pLine :: Parser Line
pLine = (LCmd <$> pCmd) <|> (LFile <$> pFile) <|> (LDir <$> pDir)

pInput :: Parser [Line]
pInput = pLine `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- case c of
--   (Cd CdRoot) -> pDirTree (DirNode "/" [] [])
--   _ -> error "pInput: no cd /"

main :: IO ()
main = do
  b <- TL.readFile "inputs/input07-sample.txt"
  print $ parseOnly pInput b
