-- |
-- Module      :  Main
-- Description :  Day 17; ?
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

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S

data Jet = L | R
  deriving (Show, Eq)

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

fall :: Rock -> Rock
fall = shiftR (0, -1)

blowAndFall :: Field -> [Jet] -> Rock -> (Field, [Jet])
blowAndFall f (j : js) r0 =
  let r1 = blow j r0
      r2 = if collided f r1 then r0 else r1
      r3 = fall r2
   in if collided f r3
        then (rest f r2, js)
        else blowAndFall f js r3
blowAndFall _ [] _ = error "fall: no jet"

fallAll :: Field -> [Jet] -> [Rock] -> [Field]
fallAll f js (r : rs) = let (f', js') = blowAndFall f js (position f r) in f' : fallAll f' js' rs
fallAll _ _ [] = error "newRock: no rock"

main :: IO ()
main = do
  d <- BS.readFile "inputs/input17.txt"
  let js = cycle $ either error id $ parseOnly pInput d
      rs = cycle rocks
      fs = fallAll field0 js rs
  print $ findYMax $ fs !! 2021
