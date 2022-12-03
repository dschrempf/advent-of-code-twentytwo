-- |
-- Module      :  Day02
-- Description :  Rock, paper, scissors
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  2 11:46:04 2022.
module Main
  ( main,
  )
where

import Control.Applicative
import Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.IO as TL

-- Part 1.

data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

data Game = Game Move Move
  deriving (Show, Eq)

class Scorable a where
  score :: a -> Int

instance Scorable Move where
  score Rock = 1
  score Paper = 2
  score Scissors = 3

instance Scorable Game where
  score (Game e p) = score p + score' e p
    where
      score' Rock Scissors = 0
      score' Scissors Rock = 6
      score' Paper Scissors = 6
      score' Scissors Paper = 0
      score' Paper Rock = 0
      score' Rock Paper = 6
      score' x y
        | x == y = 3
        | otherwise = error "score': bottom (?)"

pMoveWith :: Char -> Char -> Char -> Parser Move
pMoveWith r p s = prs Rock r <|> prs Paper p <|> prs Scissors s
  where
    prs g c = g <$ char c

pEnemy :: Parser Move
pEnemy = pMoveWith 'A' 'B' 'C'

pPlayer :: Parser Move
pPlayer = pMoveWith 'X' 'Y' 'Z'

pGame :: Parser Game
pGame = do
  e <- pEnemy
  _ <- char ' '
  p <- pPlayer
  pure $ Game e p

pInput1 :: Parser [Game]
pInput1 = pGame `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- Part 2.

data Desire = Lose | Draw | Win
  deriving (Show, Eq)

data DGame = DGame Move Desire
  deriving (Show, Eq)

moveToDesire :: Move -> Desire
moveToDesire Rock = Lose
moveToDesire Paper = Draw
moveToDesire Scissors = Win

gameToDGame :: Game -> DGame
gameToDGame (Game e p) = DGame e $ moveToDesire p

determineGame :: DGame -> Game
determineGame (DGame Rock Lose) = Game Rock Scissors
determineGame (DGame Rock Win) = Game Rock Paper
determineGame (DGame Paper Lose) = Game Paper Rock
determineGame (DGame Paper Win) = Game Paper Scissors
determineGame (DGame Scissors Lose) = Game Scissors Paper
determineGame (DGame Scissors Win) = Game Scissors Rock
determineGame (DGame x Draw) = Game x x

main :: IO ()
main = do
  b <- TL.readFile "inputs/input02.txt"
  -- Part 1.
  let gs = either error id $ parseOnly pInput1 b
      ss = map score gs
  print $ sum ss
  -- Part 2.
  let gs' = map (determineGame . gameToDGame) gs
      ss' = map score gs'
  print $ sum ss'
