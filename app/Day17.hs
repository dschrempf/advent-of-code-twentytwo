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
type Field = [Int]

field0 :: Field
field0 = replicate 7 0

position :: Field -> Rock -> Rock
position f = shiftR (2, yTarget)
  where
    yMax = maximum f
    yTarget = yMax + 4

blow :: Jet -> Rock -> Rock
blow L r | le > 0 = shiftR (-1, 0) r
  where
    le = minimum $ map fst r
blow R r | ri < 6 = shiftR (1, 0) r
  where
    ri = maximum $ map fst r
blow _ r = r

collide :: Field -> Rock -> Bool
collide f = any p
  where
    p (rX, rY) = f !! rX >= rY - 1

rest :: Field -> Rock -> Field
rest f [] = f
rest f ((x, y) : xs)
  | (f !! x) < y = let f' = take x f ++ [y] ++ drop (x + 1) f in rest f' xs
  | otherwise = rest f xs

blowAndFall :: Field -> [Jet] -> Rock -> (Field, [Jet])
blowAndFall f (j : js) r =
  let r' = blow j r
   in if collide f r'
        then (rest f r', js)
        else blowAndFall f js $ shiftR (0, -1) r'
blowAndFall _ [] _ = error "fall: no jet"

fallAll :: Field -> [Jet] -> [Rock] -> [Field]
fallAll f js (r : rs) = let (f', js') = blowAndFall f js (position f r) in f' : fallAll f' js' rs
fallAll _ _ [] = error "newRock: no rock"

main :: IO ()
main = do
  d <- BS.readFile "inputs/input17-sample.txt"
  let js = cycle $ either error id $ parseOnly pInput d
      rs = cycle rocks
      fs = fallAll field0 js rs
  print $ take 3 fs
