-- |
-- Module      :  Day04
-- Description :  Assignments
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec  4 09:55:52 2022.
module Day04
  ( main,
  )
where

import Data.Attoparsec.ByteString.Char8
import Numeric.Natural

data Assignment = Assignment Natural Natural

pAssignment :: Parser Assignment
pAssignment = do
  x <- decimal
  _ <- char '-'
  y <- decimal
  pure $ Assignment x y

pPair :: Parser (Assignment, Assignment)
pPair = undefined

main :: IO ()
main = undefined
