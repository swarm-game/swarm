-- | Simplification logic for boolean expressions that is not
-- provided in the 'boolexpr' package.
module Swarm.Game.Scenario.Objective.Simplify (
  cannotBeTrue,
  replace,
) where

import Data.BoolExpr
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S

-- | Used only by "hasContradiction".
-- Note that the Booleans returned in this tuple are not actually used
-- as conditions, and therefore their semantic convention (e.g. associating
-- True = Positive and False = Negative) is irrelevant.
-- Rather, they are collected into sets
-- to determine whether both True and False exist for a key.
extractConstFromSigned :: Signed a -> (a, Bool)
extractConstFromSigned v = case v of
  Negative x -> (x, False)
  Positive x -> (x, True)

hasContradiction :: Ord a => Conj (Signed a) -> Bool
hasContradiction (Conj items) =
  not $
    M.null $
      M.filter ((> 1) . S.size) $
        M.fromListWith (<>) $
          fmap (fmap S.singleton . extractConstFromSigned) items

simplifyDNF :: Ord a => DNF a -> DNF a
simplifyDNF (DNF (Disj disjunctions)) =
  DNF $ Disj $ L.filter (not . hasContradiction) disjunctions

isAlwaysFalse :: Ord a => DNF a -> Bool
isAlwaysFalse (DNF (Disj disjunctions)) = L.null disjunctions

cannotBeTrue :: Ord a => BoolExpr a -> Bool
cannotBeTrue = isAlwaysFalse . simplifyDNF . boolTreeToDNF

replace :: Ord a => Map a Bool -> BoolExpr a -> BoolExpr a
replace f (BAnd a b) = BAnd (replace f a) (replace f b)
replace f (BOr a b) = BOr (replace f a) (replace f b)
replace f (BNot t) = BNot (replace f t)
replace _ BTrue = BTrue
replace _ BFalse = BFalse
replace m c@(BConst x) = case M.lookup varname m of
  Nothing -> c
  Just val ->
    if txform val
      then BTrue
      else BFalse
 where
  (varname, isPositive) = extractConstFromSigned x
  txform = if isPositive then id else not
