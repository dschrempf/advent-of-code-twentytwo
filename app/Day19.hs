{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 19; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/19.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

newtype Ore = Ore Int
  deriving (Show, Eq)

pOre :: Parser Ore
pOre = Ore <$> decimal <* string " ore"

newtype Clay = Clay Int
  deriving (Show, Eq)

pClay :: Parser Clay
pClay = Clay <$> decimal <* string " clay"

newtype Obsidian = Obsidian Int
  deriving (Show, Eq)

pObsidian :: Parser Obsidian
pObsidian = Obsidian <$> decimal <* string " obsidian"

newtype Geode = Geode Int
  deriving (Show, Eq)

data Blueprint = Blueprint
  { number :: Int,
    oreRobot :: Ore,
    clayRobot :: Ore,
    obsidianRobot :: (Ore, Clay),
    geodeRobot :: (Ore, Obsidian)
  }
  deriving (Show, Eq)

pBlueprint :: Parser Blueprint
pBlueprint = do
  _ <- string "Blueprint "
  n <- decimal
  _ <- string ": Each ore robot costs "
  oreOre <- pOre
  _ <- string ". Each clay robot costs "
  oreClay <- pOre
  _ <- string ". Each obsidian robot costs "
  oreObsidian <- pOre
  _ <- string " and "
  clayObsidian <- pClay
  _ <- string ". Each geode robot costs "
  oreGeode <- pOre
  _ <- string " and "
  obsidianGeode <- pObsidian
  _ <- char '.'
  pure $ Blueprint n oreOre oreClay (oreObsidian, clayObsidian) (oreGeode, obsidianGeode)

pInput :: Parser [Blueprint]
pInput = pBlueprint `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- Part 1.

data Move = BuildOreRobot | BuildClayRobot | BuildObsidianRobot | BuildGeodeRobot | Wait

data Resources = Resources
  { ore :: Ore,
    clay :: Clay,
    obsidian :: Obsidian,
    geode :: Geode
  }

data Robots = Robots
  { oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int
  }

data State = State
  { robots :: Robots,
    resources :: Resources
  }

start :: State
start = State (Robots 1 0 0 0) (Resources (Ore 0) (Clay 0) (Obsidian 0) (Geode 0))

main :: IO ()
main = do
  d <- BS.readFile "inputs/input19-sample.txt"
  let bs = either error id $ parseOnly pInput d
  mapM_ print bs
