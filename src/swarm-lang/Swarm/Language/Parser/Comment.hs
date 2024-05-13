-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Facilities for re-inserting parsed comments back into an AST.
-- Actual parsing of comments is handled in "Swarm.Language.Parser.Lex".
module Swarm.Language.Parser.Comment (
  -- * Comment AST insertion
  populateComments,
  populateStandaloneComments,
  populateSuffixComments,

  -- * Generic tree traversals
  preorder,
  revpostorder,
) where

import Control.Lens (backwards, mapMOf, (%~))
import Control.Lens.Plated (Plated, plate)
import Control.Monad ((>=>))
import Control.Monad.State
import Data.Foldable qualified as F
import Data.List (partition)
import Data.Sequence (Seq, (<|), (|>))
import Swarm.Language.Syntax

------------------------------------------------------------
-- Comment insertion
------------------------------------------------------------

-- The approach for preserving comments is taken from
-- https://www.reddit.com/r/haskell/comments/ni4gpm/comment/gz0ipmp/ . In short:
--
--   (1) Parse all comments out-of-band and record a source span for
--       each (this is done in "Swarm.Language.Parser.Lex").
--
--   (2) For each standalone comment (i.e. comments on a line by
--       themselves), attach them to the earliest node in a preorder
--       traversal which begins after the comment.
--
--   (3) For each suffix comment (i.e. comments after something else
--       at the end of a line, or in the middle of a line), attach
--       them to the latest node in a postorder traversal which begins
--       before the comment.

-- | Re-insert parsed comments into an AST.  Prerequisite: the sequence of comments
--   must be in order by 'SrcLoc'.
populateComments :: Seq Comment -> Syntax -> Syntax
populateComments cmts = populateStandaloneComments standalone . populateSuffixComments suffix
 where
  (standalone, suffix) = partition isStandalone (F.toList cmts)

-- | Insert comments from the state at the current AST node (using the
--   provided insertion function) as long as the custom comparison
--   function returns 'True' when applied to the 'SrcLoc's of the next
--   comment and the AST node (in that order).
insertComments ::
  (SrcLoc -> SrcLoc -> Bool) ->
  (Comment -> Comments -> Comments) ->
  Syntax ->
  State [Comment] Syntax
insertComments cmpLoc ins = go
 where
  go s@(CSyntax l t cs) = do
    curCmts <- get
    case curCmts of
      [] -> return s
      (nextCmt : restCmts) -> case commentSrcLoc nextCmt `cmpLoc` l of
        True -> put restCmts >> go (CSyntax l t (ins nextCmt cs))
        False -> return s

-- | Given a list of standalone comments sorted by 'SrcLoc', insert
--   them into the given AST, attaching each comment to the earliest
--   node in a preorder traversal which begins after it.
populateStandaloneComments :: [Comment] -> Syntax -> Syntax
populateStandaloneComments cmts =
  flip evalState cmts
    . preorder (insertComments srcLocBefore (\c -> beforeComments %~ (|> c)))

-- | Given a list of suffix comments sorted by 'SrcLoc', insert
--   them into the given AST, attaching each comment to the latest
--   node in a postorder traversal which begins before it.
populateSuffixComments :: [Comment] -> Syntax -> Syntax
populateSuffixComments cmts =
  flip evalState (reverse cmts)
    . revpostorder (insertComments (flip srcLocBefore) (\c -> afterComments %~ (c <|)))

------------------------------------------------------------
-- Traversals
------------------------------------------------------------

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
--   monadic transformation.  Apply the transformation recursively to
--   all the children in reverse order, then transform the root.
--
-- >>> showTree (evalState (revpostorder next exampleTree) 0)
-- "7(6 5(4 3 2) 1(0))"
revpostorder :: (Plated a, Monad m) => (a -> m a) -> (a -> m a)
revpostorder g = go
 where
  go = mapMOf (backwards plate) go >=> g
