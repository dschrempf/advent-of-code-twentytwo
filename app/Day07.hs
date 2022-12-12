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

import Control.Applicative (Alternative ((<|>)), optional)
import Data.Attoparsec.Text.Lazy
  ( Parser,
    char,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
    string,
    takeWhile1,
  )
import Data.Char (isSpace)
import Data.Foldable (Foldable (foldl', toList), find)
import Data.List (sort, union)
import Data.Monoid (Sum (Sum))
import qualified Data.Text as TS
import qualified Data.Text.Lazy.IO as TL
import Data.Tree (Tree (Node, rootLabel))
import Numeric.Natural (Natural)

pAtom :: Parser TS.Text
pAtom = takeWhile1 (not . isSpace)

data File = File
  { fName :: TS.Text,
    fSize :: Natural
  }
  deriving (Show, Eq)

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

data DirLabel = DirLabel
  { label :: TS.Text,
    files :: [File]
  }
  deriving (Show, Eq)

type DirTree = Tree DirLabel

emptyDirTree :: TS.Text -> DirTree
emptyDirTree n = Node (DirLabel n []) []

data CdCmd = CdRoot | CdUp | CdDir TS.Text
  deriving (Show, Eq)

data Cmd = Cd CdCmd | Ls
  deriving (Show, Eq)

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
-- the sub(-sub-...)-directories are in the list, so we have to recursively go
-- through them (see 'mergeDirs').
addDir :: [DirTree] -> DirTree -> [DirTree]
addDir [] d = [d]
addDir (x : xs) d =
  if (label . rootLabel) d == (label . rootLabel) x
    then mergeDirs d x : xs
    else x : addDir xs d
  where
    mergeDirs (Node (DirLabel n fs1) ds1) (Node (DirLabel _ fs2) ds2) =
      Node (DirLabel n $ fs1 `union` fs2) $ foldl' addDir ds1 ds2

hDirTree :: DirTree -> [Line] -> (DirTree, [Line])
hDirTree d [] = (d, [])
hDirTree d@(Node l@(DirLabel n fs) ds) (x : xs) = case x of
  -- We could ignore directory listings, but to be consistent, we add an empty
  -- directory tree.
  (LDir (Dir n')) ->
    let d' = Node l (addDir ds $ emptyDirTree n')
     in hDirTree d' xs
  -- Add the file to the list.
  (LFile f) ->
    let d' = Node (DirLabel n $ fs `union` [f]) ds
     in hDirTree d' xs
  (LCmd c) -> case c of
    -- Ignore 'ls' commands.
    Ls -> hDirTree d xs
    -- Only handle directory level changes with depth 1.
    (Cd CdRoot) -> error "hDirTree: jump to root"
    (Cd CdUp) -> (d, xs)
    (Cd (CdDir n')) ->
      -- First traverse the subdirectory.
      let (d', xs') = hDirTree (emptyDirTree n') xs
          ds' = addDir ds d'
       in -- Then, continue with the current directory.
          hDirTree (Node (DirLabel n fs) ds') xs'

hInput :: [Line] -> DirTree
hInput ((LCmd (Cd CdRoot)) : xs) = fst $ hDirTree (emptyDirTree "/") xs
hInput _ = error "hInput: no cd /"

-- Part 1.

calculateDirSizes :: DirTree -> Tree Natural
calculateDirSizes (Node (DirLabel _ fs) ds) = Node (f + d) ds'
  where
    ds' = map calculateDirSizes ds
    f = sum $ map fSize fs
    d = sum $ map rootLabel ds'

-- Part 2.

totSpace :: Natural
totSpace = 70000000

needSpace :: Natural
needSpace = 30000000

findPerfectDir :: Natural -> [Natural] -> Maybe Natural
findPerfectDir totSize = find (> needDelete)
  where
    spaceLeft = totSpace - totSize
    needDelete = needSpace - spaceLeft

main :: IO ()
main = do
  b <- TL.readFile "inputs/input07.txt"
  let ls = either error id $ parseOnly pInput b
      dt = hInput ls
      st = calculateDirSizes dt
  -- Part 1.
  print $ foldMap (\s -> if s <= 100000 then Sum s else Sum 0) st
  -- Part 2.
  let ds = sort $ toList st
      totSize = rootLabel st
      thePerfectDir = findPerfectDir totSize ds
  print thePerfectDir
