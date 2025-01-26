{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for dealing with conjunctive normal form.
module Swarm.Util.CNF (
  minimizePCNF,
) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NES

type PCNF a = Set (NESet a)

-- | Minimize an expression in positive conjunctive normal form.  That
--   is, a conjunction of disjunctions like @(a1 OR a2 OR ...) AND (b1
--   OR b2 OR ...) AND ...@ in which every variable is positive (there
--   is no negation).  Minimizing means finding an expression which is
--   logically equivalent but as small as possible.
--
-- >>> import Data.Set qualified as S
-- >>> import Data.List.NonEmpty qualified as NEL
-- >>> import Data.Set.NonEmpty qualified as NES
-- >>> mpcnf = map (NEL.toList . NES.toList) . S.toList . minimizePCNF . S.fromList . mapMaybe (fmap NES.fromList . NEL.nonEmpty)
--
-- >>> mpcnf [[1 :: Int], [2,3], [1,2,3]]
-- [[1],[2,3]]
-- >>> mpcnf []
-- []
-- >>> mpcnf [[1],[1]]
-- [[1]]
-- >>> mpcnf [[1,2],[2,3],[1,2,3],[2,3,4,5],[1,2,3,4,5]]
-- [[1,2],[2,3]]
minimizePCNF :: Ord a => PCNF a -> PCNF a
minimizePCNF = S.fromList . go . sortBy (comparing NES.size) . S.toList
 where
  -- After sorting by size, we are guaranteed that c1 `isSubsetOf`
  -- c2 can only happen if c1 is earlier than c2 in the list.  So
  -- for each set we just filter out all supersets that come after
  -- it.
  --
  -- For the correctness of this algorithm, see
  -- https://math.stackexchange.com/questions/1962707/is-the-minimal-conjunctive-normal-form-for-positive-formula-unique-if-so-how-d
  go [] = []
  go (c : cs) = c : go (filter (\c2 -> not (c `NES.isSubsetOf` c2)) cs)
