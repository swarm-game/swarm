-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic effectful tree traversals.
module Swarm.Util.Traversal (preorder, revpostorder, edges) where

import Control.Lens (backwards, mapMOf)
import Control.Lens.Plated (Plated, plate)
import Control.Monad ((>=>))
import Data.Tree (Tree (..))
import GHC.Exts qualified

-- $setup
-- >>> import Control.Monad.State
-- >>> import Data.Tree
-- >>> import Data.List (intercalate)
-- >>> next :: Tree Int -> State Int (Tree Int); next (Node _ cs) = do { i <- get; put (i+1); return (Node i cs) }
-- >>> showTree :: Show a => Tree a -> String; showTree = foldTree (\n cs -> show n ++ case cs of { [] -> ""; _ -> "(" ++ intercalate " " cs ++ ")" })
-- >>> exampleTree = Node 0 [Node 0 [], Node 0 [Node 0 [], Node 0 [], Node 0 []], Node 0 [Node 0 []]]

-- | Preorder traversal of a 'Plated' structure with a monadic
--   transformation.  Apply the transformation at the root, then
--   recursively transform each of the children.
--
-- >>> showTree (evalState (preorder next exampleTree) 0)
-- "0(1 2(3 4 5) 6(7))"
preorder :: (Plated a, Monad m) => (a -> m a) -> (a -> m a)
preorder g = go
 where
  go = g >=> mapMOf plate go

-- | Reverse postorder traversal of a 'Plated' structure with a
--   monadic transformation.  Transform the root, then apply the
--   transformation recursively to all the children in reverse order.
--
-- >>> showTree (evalState (revpostorder next exampleTree) 0)
-- "0(7 3(6 5 4) 1(2))"
revpostorder :: (Plated a, Monad m) => (a -> m a) -> (a -> m a)
revpostorder g = go
 where
  go = g >=> mapMOf (backwards plate) go

-- | \(O(n)\). The edges of the tree as parent-child pairs in pre-order.
--
--   TODO(#2808): This was [added to the containers
--   package](https://hackage-content.haskell.org/package/containers-0.8.1/docs/Data-Tree.html#v:edges)
--   in version 0.8; copied here until such time as we can require containers >= 0.8.
edges :: Tree a -> [(a, a)]
edges (Node x0 ts0) = GHC.Exts.build $ \cons nil ->
  let go _ [] z = z
      go p (Node x ts : ts') z = cons (p, x) (go x ts (go p ts' z))
   in go x0 ts0 nil
{-# INLINE edges #-} -- Inline for list fusion
