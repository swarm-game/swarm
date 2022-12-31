module Data.BoolExpr.Simplify (
  cannotBeTrue
  ) where

import Data.BoolExpr
import Data.Map as M
import Data.Set as S hiding (filter)
import Data.List as L

extractConstFromSigned :: Signed a -> (a, Bool)
extractConstFromSigned v = case v of
  Negative x -> (x, False)
  Positive x -> (x, True)

hasContradiction :: Ord a => Conj (Signed a) -> Bool
hasContradiction (Conj items) =
  not $ M.null $ M.filter ((> 1) . S.size) $ M.fromListWith (<>) $
    fmap (fmap S.singleton . extractConstFromSigned) items

simplifyDNF :: Ord a => DNF a -> DNF a
simplifyDNF (DNF (Disj disjunctions)) =
  DNF $ Disj $ L.filter (not . hasContradiction) disjunctions

isAlwaysFalse :: Ord a => DNF a -> Bool
isAlwaysFalse (DNF (Disj disjunctions)) = L.null disjunctions

cannotBeTrue :: Ord a => BoolExpr a -> Bool
cannotBeTrue = isAlwaysFalse . simplifyDNF . boolTreeToDNF
