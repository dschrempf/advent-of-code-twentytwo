{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString.Char8 as BS
import Lens.Micro
import Lens.Micro.TH
import Text.Pretty.Simple

newtype Ore = Ore {_ore :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''Ore

pOre :: Parser Ore
pOre = Ore <$> decimal <* string " ore"

newtype Clay = Clay {_clay :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''Clay

pClay :: Parser Clay
pClay = Clay <$> decimal <* string " clay"

newtype Obsidian = Obsidian {_obsidian :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''Obsidian

pObsidian :: Parser Obsidian
pObsidian = Obsidian <$> decimal <* string " obsidian"

newtype Geode = Geode {_geode :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''Geode

data Blueprint = Blueprint
  { number :: Int,
    _oreRobotCost :: Ore,
    _clayRobotCost :: Ore,
    _obsidianRobotCost :: (Ore, Clay),
    _geodeRobotCost :: (Ore, Obsidian)
  }
  deriving (Show, Eq)

makeLenses ''Blueprint

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

data Instruction
  = BuildOreRobot
  | BuildClayRobot
  | BuildObsidianRobot
  | BuildGeodeRobot
  deriving (Show, Eq)

newtype OreRobot = OreRobot {_oreRobot :: Int}
  deriving (Show, Eq)

makeLenses ''OreRobot

newtype ClayRobot = ClayRobot {_clayRobot :: Int}
  deriving (Show, Eq)

makeLenses ''ClayRobot

newtype ObsidianRobot = ObsidianRobot {_obsidianRobot :: Int}
  deriving (Show, Eq)

makeLenses ''ObsidianRobot

newtype GeodeRobot = GeodeRobot {_geodeRobot :: Int}
  deriving (Show, Eq)

makeLenses ''GeodeRobot

data Robots = Robots
  { _oreRobots :: OreRobot,
    _clayRobots :: ClayRobot,
    _obsidianRobots :: ObsidianRobot,
    _geodeRobots :: GeodeRobot
  }
  deriving (Show, Eq)

makeLenses ''Robots

data Resources = Resources
  { _ores :: Ore,
    _clays :: Clay,
    _obsidians :: Obsidian,
    _geodes :: Geode
  }
  deriving (Show, Eq)

makeLenses ''Resources

data State = State
  { instruction :: Maybe Instruction,
    robots :: Robots,
    resources :: Resources
  }
  deriving (Show, Eq)

makeLenses ''State

startWith :: Instruction -> State
startWith bd = State (Just bd) ros0 res0
  where
    ros0 = Robots (OreRobot 1) (ClayRobot 0) (ObsidianRobot 0) (GeodeRobot 0)
    res0 = Resources (Ore 0) (Clay 0) (Obsidian 0) (Geode 0)

harvest :: Robots -> Resources -> Resources
harvest ros res =
  res
    & ores . ore %~ (+ ros ^. oreRobots . oreRobot)
    & clays . clay %~ (+ ros ^. clayRobots . clayRobot)
    & obsidians . obsidian %~ (+ ros ^. obsidianRobots . obsidianRobot)
    & geodes . geode %~ (+ ros ^. geodeRobots . geodeRobot)

canAfford :: Blueprint -> Instruction -> Resources -> Bool
canAfford bp BuildOreRobot res = bp ^. oreRobotCost <= res ^. ores
canAfford bp BuildClayRobot res = bp ^. clayRobotCost <= res ^. ores
canAfford bp BuildObsidianRobot res =
  bp ^. obsidianRobotCost . _1 <= res ^. ores
    && bp ^. obsidianRobotCost . _2 <= res ^. clays
canAfford bp BuildGeodeRobot res =
  bp ^. geodeRobotCost . _1 <= res ^. ores
    && bp ^. geodeRobotCost . _2 <= res ^. obsidians

addRobot :: Instruction -> Robots -> Robots
addRobot bd ros = ros & l %~ (+ 1)
  where
    l = case bd of
      BuildOreRobot -> oreRobots . oreRobot
      BuildClayRobot -> clayRobots . clayRobot
      BuildObsidianRobot -> obsidianRobots . obsidianRobot
      BuildGeodeRobot -> geodeRobots . geodeRobot

subtractResources :: Blueprint -> Instruction -> Resources -> Resources
subtractResources bp BuildOreRobot res =
  res & ores . ore %~ subtract (bp ^. oreRobotCost . ore)
subtractResources bp BuildClayRobot res =
  res & ores . ore %~ subtract (bp ^. clayRobotCost . ore)
subtractResources bp BuildObsidianRobot res =
  res
    & ores . ore %~ subtract (bp ^. obsidianRobotCost . _1 . ore)
    & clays . clay %~ subtract (bp ^. obsidianRobotCost . _2 . clay)
subtractResources bp BuildGeodeRobot res =
  res
    & ores . ore %~ subtract (bp ^. geodeRobotCost . _1 . ore)
    & obsidians . obsidian %~ subtract (bp ^. geodeRobotCost . _2 . obsidian)

-- Return 'Nothing' if not enough resources are available.
build :: Blueprint -> Instruction -> Robots -> Resources -> (Robots, Resources)
build bp bd ros res = (addRobot bd ros, subtractResources bp bd res)

lap :: Blueprint -> State -> State
lap bp (State mbd ros res) = case mbd of
  (Just bd)
    -- First check if we can afford the robot.
    | canAfford bp bd res ->
        -- If so, first harvest, then add the robot.
        let res' = harvest ros res
            (ros', res'') = build bp bd ros res'
         in State Nothing ros' res''
    | otherwise -> State mbd ros $ harvest ros res
  Nothing -> error "lap: no instruction"

main :: IO ()
main = do
  d <- BS.readFile "inputs/input19-sample.txt"
  let bs = either error id $ parseOnly pInput d
      s0 = startWith BuildOreRobot
      bp = head bs
      f = lap bp
  pPrint $ take 6 $ iterate f s0
