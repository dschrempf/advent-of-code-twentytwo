-- |
-- Module      :  Main
-- Description :  Day 20; Grove Positioning System
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  9 09:10:38 2022.
--
-- See https://adventofcode.com/2022/day/20.
module Main
  ( main,
  )
where

import Aoc.Function
import Control.Applicative (optional)
import Control.Monad.ST
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    sepBy1',
    signed,
  )
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (foldl')
import Data.List (elemIndex)
import Data.Massiv.Array
  ( Comp (Seq),
    Ix1,
    MVector,
    Size (size),
    Sz (Sz),
    U (U),
    Vector,
    computeAs,
    drop,
    freezeS,
    fromList,
    imapM_,
    makeArray,
    map,
    modify_,
    sappend,
    sconcat,
    singleton,
    take,
    thawS,
    toList,
    (!),
  )
import Data.Massiv.Core (PrimMonad (..))
import Data.Maybe (fromJust)
import Prelude hiding (drop, map, take)

-- Array of numbers and their current indices.
type Xs = Vector U (Int, Ix1)

type XsM m = MVector (PrimState m) U (Int, Ix1)

-- Shifted array of original indices.
type Is = Vector U Ix1

pInput :: Parser [Int]
pInput = signed decimal `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

-- Part 1.

updateIs :: Is -> Xs -> Xs
updateIs is xs = runST $ do
  m <- thawS xs
  imapM_ (updateI m) is
  freezeS m

updateI :: PrimMonad m => XsM m -> Ix1 -> Ix1 -> m ()
updateI m iNew = modify_ m f
  where
    f (x, _) = pure (x, iNew)

shift ::
  (Xs, Is) ->
  -- Original index of element being shifted.
  Int ->
  (Xs, Is)
shift (xs, is) i0 = (xsNew, isNew)
  where
    (Sz l) = size xs
    (x, iNow) = xs ! i0
    -- Index vector without the current element.
    isNoX = computeAs U $ take (Sz iNow) is `sappend` drop (Sz $ succ iNow) is
    -- Current element of index vector.
    lm1 = pred l
    iNew = case (iNow + x) `mod` lm1 of
      n
        | n == 0 -> lm1
        | n == lm1 -> 0
        | otherwise -> n
    isNew = computeAs U $ sconcat [take (Sz iNew) isNoX, singleton i0, drop (Sz iNew) isNoX]
    xsNew = updateIs isNew xs

getShifted :: (Xs, Is) -> Vector U Int
getShifted (xs, is) = makeArray Seq (size xs) f
  where
    f i = let i0 = is ! i in fst $ xs ! i0

getSol :: Vector U Int -> Int
getSol xs = sum [xs ! ((i0 + n) `mod` l) | n <- ns]
  where
    (Sz l) = size xs
    i0 = fromJust $ elemIndex 0 $ toList xs
    ns = [1000, 2000, 3000]

-- Part 2.

main :: IO ()
main = do
  d <- BS.readFile "inputs/input20.txt"
  -- Part 1.
  let ls = either error id $ parseOnly pInput d
      xs :: Xs
      xs = fromList Seq $ zip ls [0 ..]
      (Sz l) = size xs
      is :: Is
      is = fromList Seq [0 .. pred l]
      s0 = (xs, is)
      s = foldl' shift s0 [0 .. pred l]
  -- let ss = scanl' shift s0 [0 .. pred l]
  --     f x = (x, getShifted x)
  -- mapM_ (print . f) ss
  print $ getSol $ getShifted s
  -- Part 2.
  let f (x, i) = (811589153 * x, i)
      xs' = computeAs U $ map f xs
      s02 = (xs', is)
      sOnce x = foldl' shift x [0 .. pred l]
  print $ getSol $ getShifted $ nTimesStrict 10 sOnce s02
