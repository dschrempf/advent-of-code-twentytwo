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
import Data.List (scanl')
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
    modify_,
    sappend,
    sconcat,
    singleton,
    take,
    thawS,
    (!),
  )
import Data.Massiv.Core (PrimMonad (..))
import Debug.Trace
import Prelude hiding (drop, take)

-- Array of numbers and their current indices.
type Xs = Vector U (Int, Ix1)

type XsM m = MVector (PrimState m) U (Int, Ix1)

-- Shifted array of original indices.
type Is = Vector U Ix1

pInput :: Parser [Int]
pInput = signed decimal `sepBy1'` endOfLine <* optional endOfLine <* endOfInput

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
    iNew = traceShowId $ case (iNow + x) `mod` lm1 of
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

main :: IO ()
main = do
  d <- BS.readFile "inputs/input20-sample.txt"
  let ls = either error id $ parseOnly pInput d
      xs :: Xs
      xs = fromList Seq $ zip ls [0 ..]
      (Sz l) = size xs
      is :: Is
      is = fromList Seq [0 .. pred l]
      s0 = (xs, is)
      ss = scanl' shift s0 [0 .. pred l]
      f x = (x, getShifted x)
  mapM_ (print . f) ss
