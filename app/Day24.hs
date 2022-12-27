-- |
-- Module      :  Main
-- Description :  Day 24; ?
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/24.
module Main
  ( main,
  )
where

import Control.Applicative (Alternative (..), optional, (<|>))
import Control.Monad.ST (runST)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
  )
import qualified Data.ByteString.Char8 as BS
import Data.Char (intToDigit)
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as N
import Data.Massiv.Array
  ( Array,
    B,
    Comp (..),
    Ix2 (..),
    MArray,
    PrimMonad (..),
    Sz (..),
    (!>),
  )
import qualified Data.Massiv.Array as A
import Data.Maybe (fromJust)

data Blizzard = North | West | South | East
  deriving (Show, Eq)

toCharB :: Blizzard -> Char
toCharB North = '^'
toCharB West = '<'
toCharB South = 'v'
toCharB East = '>'

pBlizzard :: Parser Blizzard
pBlizzard =
  North <$ char '^'
    <|> West <$ char '<'
    <|> South <$ char 'v'
    <|> East <$ char '>'

data Cell = BlizzardC (NonEmpty Blizzard) | Ground | Boundary
  deriving (Show, Eq)

toChar :: Cell -> Char
toChar (BlizzardC xs)
  | l == 1 = toCharB $ N.head xs
  | otherwise = intToDigit l
  where
    l = length xs
toChar Ground = '.'
toChar Boundary = '#'

pCell :: Parser Cell
pCell =
  BlizzardC . N.singleton <$> pBlizzard
    <|> Ground <$ char '.'
    <|> Boundary <$ char '#'

type F2 = Array B Ix2 Cell

showField :: F2 -> String
showField xs = unlines $ map (map toChar) $ A.toLists xs

pLine :: Parser [Cell]
pLine = some pCell

pField :: Parser F2
pField = A.fromLists' Par <$> pLine `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

type F2M m = MArray (PrimState m) B Ix2 Cell

getBlueprint :: F2 -> F2
getBlueprint = fmap erase
  where
    erase Boundary = Boundary
    erase _ = Ground

addB :: PrimMonad m => Blizzard -> Cell -> m Cell
addB b Ground = pure $ BlizzardC $ N.singleton b
addB b (BlizzardC xs) = pure $ BlizzardC $ b <| xs
addB b Boundary = error "addB: blizzard was blown into boundary"

blowB :: PrimMonad m => F2M m -> Ix2 -> Blizzard -> m ()
blowB xs (Ix2 r c) b = A.modify_ xs (addB b) ix'
  where
    (Sz2 m n) = A.sizeOfMArray xs
    ix' = case b of
      North -> Ix2 (predWithWrap m r) c
      West -> Ix2 r (predWithWrap n c)
      South -> Ix2 (succWithWrap m r) c
      East -> Ix2 r (succWithWrap n c)
    predWithWrap xMax x
      | xm1 <= 0 = xMax - 2
      | otherwise = xm1
      where
        xm1 = pred x
    succWithWrap xMax x
      | xp1 >= pred xMax = 1
      | otherwise = xp1
      where
        xp1 = succ x

blowC :: PrimMonad m => F2M m -> Ix2 -> Cell -> m ()
blowC xs ix (BlizzardC bs) = mapM_ (blowB xs ix) bs
blowC _ _ _ = pure ()

blow :: F2 -> F2 -> F2
blow blueprint xs = runST $ do
  m <- A.thawS blueprint
  A.imapM_ (blowC m) xs
  A.freezeS m

getStart :: F2 -> Ix2
getStart = fromJust . A.findIndex (== Ground)

getEnd :: F2 -> Ix2
getEnd xs = Ix2 r $ fromJust $ A.findIndex (== Ground) $ xs !> pred r
  where
    (Sz2 r c) = A.size xs

main :: IO ()
main = do
  d <- BS.readFile "inputs/input24-sample.txt"
  let xs = either error id $ parseOnly pField d
  putStr $ showField xs
  let p0 = getStart xs
      pe = getEnd xs
  print (p0, pe)
  let bp = getBlueprint xs
  putStr $ showField $ blow bp xs
