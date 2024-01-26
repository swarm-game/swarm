#!/usr/bin/env stack
{- stack script --resolver lts-21.25
  --package data-ordlist
  --package split
-}

import Data.List.Split (splitOn)
import Data.List.Ordered (nubSort)

-- |
-- Extracts all "conids" from a list of "modids".
-- (see definitions here: https://www.haskell.org/onlinereport/haskell2010/haskellch5.html )
--
-- E.g., takes lines of the following form:
--
--    Data.Text
--    Control.Arrow
--
-- and produces a flattened list of words:
--
--   Arrow
--   Control
--   Data
--   Text
splitParts = unlines . nubSort . concatMap (splitOn ".") . lines

main :: IO ()
main = interact splitParts
