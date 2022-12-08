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
import Data.List
import qualified Data.Text as TS
import qualified Data.Text.Lazy.IO as TL

data File = File
  { fName :: TS.Text,
    fSize :: Integer
  }
  deriving (Show, Eq)

data DirTree = DirNode
  { label :: TS.Text,
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
  deriving (Show)

pDir :: Parser Dir
pDir = string "dir " *> (Dir <$> pAtom)

data Line = LCmd Cmd | LFile File | LDir Dir
  deriving (Show)

pLine :: Parser Line
pLine = (LCmd <$> pCmd) <|> (LFile <$> pFile) <|> (LDir <$> pDir)

pInput :: Parser [Line]
pInput = pLine `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- Add a directory to an existing list of directories.
--
-- Meh, this function turned out to be a bit complicated. The problem is, we do
-- not know if the directory is already in the list. Also, we do not know if all
-- the sub-(sub-...) are in the list, so we have to recursively go through them
-- (see 'mergeDirs').
addDir :: [DirTree] -> DirTree -> [DirTree]
addDir [] d = [d]
addDir (x : xs) d =
  if label d == label x
    then mergeDirs d x : xs
    else x : addDir xs d
  where
    mergeDirs (DirNode n fs1 ds1) (DirNode _ fs2 ds2) =
      DirNode n (fs1 `union` fs2) $ foldl' addDir ds1 ds2

hDirTree :: DirTree -> [Line] -> (DirTree, [Line])
hDirTree d@(DirNode n fs ds) (x : xs) = case x of
  (LCmd c) -> case c of
    -- Ignore 'ls' commands.
    Ls -> hDirTree d xs
    -- Only handle one level changes.
    (Cd CdRoot) -> error "hDirTree: jump to root"
    (Cd CdUp) -> (d, xs)
    (Cd (CdDir n')) ->
      let (d', xs') = hDirTree (DirNode n' [] []) xs
          ds' = addDir ds d'
       in (DirNode n fs ds', xs')

hInput :: [Line] -> DirTree
hInput ((LCmd (Cd CdRoot)) : xs) = fst $ hDirTree (DirNode "/" [] []) xs
hInput _ = error "hInput: no cd /"

main :: IO ()
main = do
  b <- TL.readFile "inputs/input07-sample.txt"
  let ls = either error id $ parseOnly pInput b
  print $ hInput ls
