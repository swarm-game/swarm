{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for working with comments in Swarm prgramming language.
module Swarm.Language.Syntax.Comments (
  CommentType (..),
  CommentSituation (..),
  isStandalone,
  Comment (..),
  Comments (..),
  beforeComments,
  afterComments,
) where

import Control.Lens (AsEmpty, makeLenses)
import Data.Aeson.Types hiding (Key)
import Data.Data (Data)
import Data.Sequence (Seq)
import Data.Text hiding (filter, length, map)
import GHC.Generics (Generic)
import Swarm.Language.Syntax.Loc

-- | Line vs block comments.
data CommentType = LineComment | BlockComment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Data, ToJSON, FromJSON)

-- | Was a comment all by itself on a line, or did it occur after some
--   other tokens on a line?
data CommentSituation = StandaloneComment | SuffixComment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Data, ToJSON, FromJSON)

-- | Test whether a comment is a standalone comment or not.
isStandalone :: Comment -> Bool
isStandalone = (== StandaloneComment) . commentSituation

-- | A comment is retained as some text plus metadata (source
--   location, comment type, + comment situation).  While parsing we
--   record all comments out-of-band, for later re-insertion into the
--   AST.
data Comment = Comment
  { commentSrcLoc :: SrcLoc
  , commentType :: CommentType
  , commentSituation :: CommentSituation
  , commentText :: Text
  }
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON)

-- | Comments which can be attached to a particular AST node.  Some
--   comments come textually before the node and some come after.
data Comments = Comments
  { _beforeComments :: Seq Comment
  , _afterComments :: Seq Comment
  }
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON)

makeLenses ''Comments

instance Semigroup Comments where
  Comments b1 a1 <> Comments b2 a2 = Comments (b1 <> b2) (a1 <> a2)

instance Monoid Comments where
  mempty = Comments mempty mempty

instance AsEmpty Comments
