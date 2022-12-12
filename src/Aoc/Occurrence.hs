-- |
-- Module      :  Aoc.Occurrence
-- Description :  Count occurrences of things
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Dec 12 09:37:29 2022.
module Aoc.Occurrence
  ( OccurrenceMap,
    count,
  )
where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M

type OccurrenceMap a = M.Map a Int

addElem :: Ord a => OccurrenceMap a -> a -> OccurrenceMap a
addElem m k = M.insertWith (const succ) k 1 m

count :: (Foldable f, Ord a) => f a -> OccurrenceMap a
count = foldl' addElem M.empty
