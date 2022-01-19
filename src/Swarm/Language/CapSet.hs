-- |
-- Module      :  Swarm.Language.CapSet
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
module Swarm.Language.CapSet where

import Control.Arrow ((&&&), (***), (>>>))
import Data.Either (partitionEithers)
import Data.List (foldl')
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Swarm.Language.Capability (Capability)
import Swarm.Language.Context (Var)
import Prelude as P

data Subset v c = Subset {vars :: Set v, consts :: Set c}
  deriving (Eq, Ord, Show)
data Ineq v c = Subset v c :<: Subset v c
  deriving (Eq, Ord, Show)
type System v c = [Ineq v c]

type CapSet = Subset Var Capability

null :: Subset v c -> Bool
null (Subset vs cs) = S.null vs && S.null cs

empty :: Subset v c
empty = Subset S.empty S.empty

fromList :: (Ord v, Ord c) => [Either v c] -> Subset v c
fromList = partitionEithers >>> (S.fromList *** S.fromList) >>> uncurry Subset

toList :: Subset v c -> [c]
toList = S.toList . consts

insert :: Ord c => c -> Subset v c -> Subset v c
insert c (Subset vs cs) = Subset vs (S.insert c cs)

singleton :: c -> Subset v c
singleton c = Subset S.empty (S.singleton c)

var :: v -> Subset v c
var v = Subset (S.singleton v) S.empty

union :: (Ord v, Ord c) => Subset v c -> Subset v c -> Subset v c
union (Subset vs1 cs1) (Subset vs2 cs2) = Subset (S.union vs1 vs2) (S.union cs1 cs2)

unions :: (Ord v, Ord c) => [Subset v c] -> Subset v c
unions = foldl' union empty

eval :: (Ord v, Ord c) => Map v (Set c) -> Subset v c -> Set c
eval m (Subset vs cs) = cs `S.union` (S.unions . map (m !) $ S.toList vs)

evalIneq :: (Ord v, Ord c) => Map v (Set c) -> Ineq v c -> Bool
evalIneq m (s1 :<: s2) = eval m s1 `S.isSubsetOf` eval m s2

solve :: (Ord v, Ord c) => System v c -> Maybe (Map v (Set c))
solve s = go initialMap
 where
  allVars = S.unions . map (\(lhs :<: rhs) -> vars lhs `S.union` vars rhs) $ s
  initialMap = M.fromList (zip (S.toList allVars) (repeat S.empty))

  go m
    | P.null rs = Just m
    | otherwise = do
      fixes <- mapM resolve rs
      go (foldr (>>>) id fixes m)
   where
    rs = filter (not . P.null . snd) . map (id &&& residual) $ s
    residual (s1 :<: s2) = eval m s1 `S.difference` eval m s2

    resolve (_ :<: s2, res) = case S.toList (vars s2) of
      [] -> Nothing
      (v : _) -> Just $ M.adjust (S.union res) v
