{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Main
-- Description :  Day 19; Not Enough Minerals
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
--
-- I tried to use lenses for this project, but oh my ..., it looks ugly.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Function
import Data.Maybe
import qualified Data.Set as S
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
  deriving (Show, Eq, Bounded, Enum, Ord)

newtype OreRobot = OreRobot {_oreRobot :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''OreRobot

newtype ClayRobot = ClayRobot {_clayRobot :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''ClayRobot

newtype ObsidianRobot = ObsidianRobot {_obsidianRobot :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''ObsidianRobot

newtype GeodeRobot = GeodeRobot {_geodeRobot :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''GeodeRobot

data Robots = Robots
  { _oreRobots :: OreRobot,
    _clayRobots :: ClayRobot,
    _obsidianRobots :: ObsidianRobot,
    _geodeRobots :: GeodeRobot
  }
  deriving (Show, Eq, Ord)

makeLenses ''Robots

data Resources = Resources
  { _ores :: Ore,
    _clays :: Clay,
    _obsidians :: Obsidian,
    _geodes :: Geode
  }
  deriving (Show, Eq, Ord)

makeLenses ''Resources

data State = State
  { minute :: Int,
    instruction :: Maybe Instruction,
    _robots :: Robots,
    _resources :: Resources
  }
  deriving (Show, Eq, Ord)

makeLenses ''State

start :: Blueprint -> S.Set State
start bp = instruct bp (getBlueprintSpec bp) $ State 0 Nothing ros0 res0
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
lap bp (State n mbd ros res) = case mbd of
  (Just bd)
    -- First check if we can afford the robot.
    | canAfford bp bd res ->
        -- If so, first harvest, then add the robot.
        let res' = harvest ros res
            (ros', res'') = build bp bd ros res'
         in State n Nothing ros' res''
    | otherwise -> State n mbd ros $ harvest ros res
  Nothing -> error "lap: no instruction"

instruct :: Blueprint -> BlueprintSpec -> State -> S.Set State
instruct bp (BlueprintSpec orMax clMax obMax) (State n mbd ros res) = case mbd of
  Nothing
    | canAfford bp BuildGeodeRobot res -> S.singleton $ State n' (Just BuildGeodeRobot) ros res
    -- -- The following trick worked for part 1, but not for part 2.
    -- \| canAfford bp BuildObsidianRobot res -> S.singleton $ State n' (Just BuildObsidianRobot) ros res
    | otherwise ->
        -- Only build obsidian and geode robots if resources are being
        -- harvested. Do not build more robots than necessary.
        let oreR =
              if ros ^. oreRobots . oreRobot < orMax ^. ore
                then Just BuildOreRobot
                else Nothing
            claR =
              if ros ^. clayRobots . clayRobot < clMax ^. clay
                then Just BuildClayRobot
                else Nothing
            obsR =
              if ros ^. clayRobots . clayRobot > 0
                && ros ^. obsidianRobots . obsidianRobot < obMax ^. obsidian
                then Just BuildObsidianRobot
                else Nothing
            geoR =
              if ros ^. obsidianRobots . obsidianRobot > 0
                then Just BuildGeodeRobot
                else Nothing
            bds = catMaybes [oreR, claR, obsR, geoR]
         in S.fromList [State n' (Just bd) ros res | bd <- bds]
  Just bd -> S.singleton $ State n' (Just bd) ros res
  where
    n' = n + 1

-- This helps A LOT. Basically, throw out all states that cannot beat the best
-- state, even when they build a geode robot each turn, from now on.
prune :: Int -> S.Set State -> S.Set State
prune timeMax xs = S.filter p xs
  where
    bestSoFar = maximumBy (compare `on` (_geodes . _resources)) xs
    timeLeft = timeMax - minute bestSoFar
    possible = timeLeft * (timeLeft - 1) `div` 2
    getMaxG x =
      let m = x ^. resources . geodes . geode
          r = x ^. robots . geodeRobots . geodeRobot
       in m + r * timeLeft + possible
    bestG =
      let m = bestSoFar ^. resources . geodes . geode
          r = bestSoFar ^. robots . geodeRobots . geodeRobot
       in m + r * timeLeft
    p x = getMaxG x >= bestG

findBest :: Int -> Blueprint -> Geode
findBest n bp = maximum $ S.map (_geodes . _resources) $ iterate r s0 !! n
  where
    s0 = start bp
    l = lap bp
    i = instruct bp (getBlueprintSpec bp)
    r = prune n . S.unions . S.map (i . l)

-- -- I used these functions while working on the puzzle.
-- findAllS :: Int -> Blueprint -> S.Set State
-- findAllS n bp = iterate r s0 !! n
--   where
--     s0 = start bp
--     l = lap bp
--     i = instruct bp (getBlueprintSpec bp)
--     r = S.unions . S.map (i . l)

-- findBestS :: Int -> Blueprint -> State
-- findBestS n bp = maximumBy (compare `on` (_geodes . _resources)) $ findAllS n bp

data BlueprintSpec = BlueprintSpec
  { _maxOre :: Ore,
    _maxClay :: Clay,
    _maxObsidian :: Obsidian
  }
  deriving (Show)

getBlueprintSpec :: Blueprint -> BlueprintSpec
getBlueprintSpec (Blueprint _ _ ore2 (_, cla) (_, obs)) =
  BlueprintSpec oreMax cla obs
  where
    -- We won't get to building obsidian or geode robots each turn, so it is
    -- enough to be able to build clay robots each turn.
    oreMax = ore2

main :: IO ()
main = do
  d <- BS.readFile "inputs/input19.txt"
  let bs = either error id $ parseOnly pInput d
  -- Part 1.
  let bests1 = map (findBest 24) bs
  pPrint $ sum $ zipWith ((*) . _geode) bests1 [1 ..]
  -- Part 2.
  pPrint $ product $ map (_geode . findBest 32) $ take 3 bs
