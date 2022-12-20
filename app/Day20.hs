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

import Control.Applicative (optional)
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
import Data.Massiv.Array
  ( Comp (Seq),
    Ix1,
    Size (size),
    Sz (Sz),
    U (U),
    Vector,
    computeAs,
    computeS,
    drop,
    fromList,
    sappend,
    sconcat,
    singleton,
    take,
    (!),
  )
import Prelude hiding (drop, take)

-- Array of numbers and their current indices.
type Xs = Vector U (Int, Ix1)

-- Shifted array of original indices.
type Is = Vector U Ix1

pInput :: Parser [Int]
pInput = signed decimal `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

shift ::
  -- Original index of element being shifted.
  Int ->
  Xs ->
  Is ->
  (Xs, Is)
shift i0 xs is = (undefined, isNew)
  where
    (Sz l) = size xs
    (x, iNow) = xs ! i0
    -- Current element of index vector.
    iNew = (iNow + x) `mod` pred l
    -- Index vector without the current element.
    isNoX = computeAs U $ take (Sz iNow) is `sappend` drop (Sz $ succ iNow) is
    isNew = computeAs U $ sconcat [take (Sz iNew) isNoX, singleton i0, drop (Sz $ succ iNew) isNoX]

main :: IO ()
main = do
  d <- BS.readFile "inputs/input20-sample.txt"
  let ls = either error id $ parseOnly pInput d
      xs :: Xs
      xs = fromList Seq $ zip ls [0 ..]
      (Sz l) = size xs
      is :: Is
      is = fromList Seq [0 .. pred l]
  print xs
