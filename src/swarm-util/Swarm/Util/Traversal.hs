-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic effectful tree traversals.
module Swarm.Util.Traversal (preorder, revpostorder) where

import Control.Lens (backwards, mapMOf)
import Control.Lens.Plated (Plated, plate)
import Control.Monad ((>=>))

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
