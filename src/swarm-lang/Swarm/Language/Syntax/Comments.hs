{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for working with comments in Swarm programming language.
module Swarm.Language.Syntax.Comments (
  CommentType (..),
  CommentSituation (..),
  isStandalone,
  Comment (..),
  Comments (..),
  beforeComments,
  afterComments,
) where

import Control.Lens (AsEmpty, makeLenses, pattern Empty)
import Data.Aeson qualified as A
import Data.Aeson.Types hiding (Key)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (pretty)
import Swarm.Language.Syntax.Loc
import Swarm.Pretty (PrettyPrec (..))

-- | Line vs block comments.
data CommentType = LineComment | BlockComment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Data, Hashable, ToJSON, FromJSON)

-- | Was a comment all by itself on a line, or did it occur after some
--   other tokens on a line?
data CommentSituation = StandaloneComment | SuffixComment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Data, Hashable, ToJSON, FromJSON)

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
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON, Hashable)

instance PrettyPrec Comment where
  prettyPrec _ (Comment _ LineComment _ txt) = "//" <> pretty txt
  prettyPrec _ (Comment _ BlockComment _ txt) = "/*" <> pretty txt <> "*/"

-- | Comments which can be attached to a particular AST node.  Some
--   comments come textually before the node and some come after.
data Comments = Comments
  { _beforeComments :: Seq Comment
  , _afterComments :: Seq Comment
  }
  deriving (Eq, Show, Generic, Data, Hashable)

makeLenses ''Comments

instance ToJSON Comments where
  toJSON = A.genericToJSON A.defaultOptions
  omitField = \case
    Empty -> True
    _ -> False

instance FromJSON Comments where
  parseJSON = A.genericParseJSON A.defaultOptions
  omittedField = Just Empty

instance Semigroup Comments where
  Comments b1 a1 <> Comments b2 a2 = Comments (b1 <> b2) (a1 <> a2)

instance Monoid Comments where
  mempty = Comments mempty mempty

instance AsEmpty Comments
