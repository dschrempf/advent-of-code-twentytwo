{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Main
-- Description :  Day 17; Pyroclastic Flow
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

import Aoc.List (chop, findCycle)
import Control.Applicative (Alternative (some, (<|>)), optional)
import Control.DeepSeq (NFData, deepseq, force)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    endOfInput,
    endOfLine,
    parseOnly,
  )
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Set as S
import GHC.Generics (Generic)

data Jet = L | R
  deriving (Show, Eq, Generic)

instance NFData Jet

pJet :: Parser Jet
pJet = (L <$ char '<') <|> (R <$ char '>')

pInput :: Parser [Jet]
pInput = some pJet <* optional endOfLine <* endOfInput

type Position = (Int, Int)

type Rock = [Position]

rock1 :: Rock
rock1 = [(0, 0), (1, 0), (2, 0), (3, 0)]

rock2 :: Rock
rock2 = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

rock3 :: Rock
rock3 = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

rock4 :: Rock
rock4 = [(0, 0), (0, 1), (0, 2), (0, 3)]

rock5 :: Rock
rock5 = [(0, 0), (1, 0), (0, 1), (1, 1)]

rocks :: [Rock]
rocks = [rock1, rock2, rock3, rock4, rock5]

shiftP :: (Int, Int) -> Position -> Position
shiftP (dx, dy) (x, y) = (x + dx, y + dy)

shiftR :: (Int, Int) -> Rock -> Rock
shiftR d = map (shiftP d)

-- Only store the 7 highest positions.
type Field = S.Set Position

field0 :: Field
field0 = S.fromList $ zip [0 .. 6] $ repeat 0

findYMax :: Field -> Int
findYMax = maximum . S.map snd

position :: Field -> Rock -> Rock
position f = shiftR (2, yTarget)
  where
    yMax = findYMax f
    yTarget = yMax + 4

collided :: Field -> Rock -> Bool
collided zs r = collideWithLeftBoundary || collideWithRightBoundary || collideWithField
  where
    xs = map fst r
    collideWithLeftBoundary = minimum xs < 0
    collideWithRightBoundary = maximum xs > 6
    collideWithField = any (`S.member` zs) r

blow :: Jet -> Rock -> Rock
blow L r = shiftR (-1, 0) r
blow R r = shiftR (1, 0) r

rest :: Field -> Rock -> Field
rest f = S.union f . S.fromList

fallOne :: Rock -> Rock
fallOne = shiftR (0, -1)

blowAndFall :: Field -> [Jet] -> Rock -> (Field, [Jet])
blowAndFall f (j : js) r0 =
  let r1 = blow j r0
      r2 = if collided f r1 then r0 else r1
      r3 = fallOne r2
   in if collided f r3
        then (cleanField $ rest f r2, js)
        else blowAndFall f js r3
blowAndFall _ [] _ = error "fall: no jet"

-- Only keep important fields.
cleanField :: Field -> Field
cleanField xs = S.filter (\(_, y) -> y >= minY) xs
  where
    maxI i = maximum $ S.map snd $ S.filter ((== i) . fst) xs
    maxima = map maxI [0 .. 6]
    minY = minimum maxima - 100

fall :: ([Jet], [Rock], Field) -> ([Jet], [Rock], Field)
fall (js, r : rs, f) =
  let (f', js') = blowAndFall f js (position f r)
   in (js', rs, force f')
fall (_, [], _) = error "newRock: no rock"

ix :: Int
ix = 1000000000000

nTimes :: [Jet] -> [Rock] -> Int -> Field -> Field
nTimes js rs n f
  | n <= 0 = f
  | otherwise =
      let (js', rs', f') = fall (js, rs, f)
       in deepseq f' $ nTimes js' rs' (n - 1) f'

maxLength :: Int
maxLength = 10000

shift :: Int
shift = 10000

main :: IO ()
main = do
  d <- BS.readFile "inputs/input17.txt"
  -- Part 1.
  let jj = either error id $ parseOnly pInput d
      js = cycle jj
      rs = cycle rocks
      f0 = (js, rs, field0)
      fm (_, _, f) = findYMax f
  print $ findYMax $ nTimes js rs 2022 field0
  -- Part 2.
  let hs = map fm $ iterate fall f0
      ds = zipWith (-) (tail hs) hs
      (cycleLength, cycles) = fromJust $ findCycle maxLength $ drop shift ds
  let beginning = sum $ take shift ds
      nLeft = ix - shift
      (ns, r) = nLeft `divMod` cycleLength
  print $ beginning + ns * sum cycles + sum (take r cycles)
