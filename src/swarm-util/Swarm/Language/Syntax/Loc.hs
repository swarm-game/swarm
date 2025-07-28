{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for working with locations of something in source code.
module Swarm.Language.Syntax.Loc (
  SrcLoc (..),
  Located (..),
  srcLocStartsBefore,
  srcLocEndsBefore,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Swarm.Language.Syntax.Import (ImportLoc, Resolved)
import Swarm.Util.JSON (optionsUntagged)

------------------------------------------------------------
-- SrcLoc
------------------------------------------------------------

-- | The location of something in the textual source code (recorded as
--   an import location, along with an interval measured in terms of
--   indices into the input stream).
data SrcLoc
  = NoLoc
  | -- | Half-open interval from start (inclusive) to end (exclusive)
    SrcLoc (Maybe (ImportLoc Resolved)) Int Int
  deriving (Eq, Ord, Show, Generic, Data, Hashable)

instance FromJSON SrcLoc where
  parseJSON = genericParseJSON optionsUntagged

instance ToJSON SrcLoc where
  toJSON = genericToJSON optionsUntagged
  omitField = (== NoLoc)

-- | @x <> y@ is the smallest 'SrcLoc' that subsumes both @x@ and @y@,
--   or @NoLoc@ if this is not possible (i.e. if either one is
--   @NoLoc@, or if they come from different import locations).
instance Semigroup SrcLoc where
  NoLoc <> l = l
  l <> NoLoc = l
  SrcLoc l1 s1 e1 <> SrcLoc l2 s2 e2
    | l1 /= l2 = NoLoc
    | otherwise = SrcLoc l1 (min s1 s2) (max e1 e2)

-- | @mempty@ is a special value which means we have no location
--   information.
instance Monoid SrcLoc where
  mempty = NoLoc

-- | Check whether one @SrcLoc@ starts before another one,
--   /i.e./ compare their starting indices to see if the first is @<=@
--   the second.
srcLocStartsBefore :: SrcLoc -> SrcLoc -> Bool
srcLocStartsBefore (SrcLoc l1 a _) (SrcLoc l2 b _) = l1 == l2 && a <= b
srcLocStartsBefore _ _ = False

-- | Check whether the first @SrcLoc@ ends before the second, /i.e./
--   compare their ending indices to see if the first is @<=@ the
--   second.
srcLocEndsBefore :: SrcLoc -> SrcLoc -> Bool
srcLocEndsBefore (SrcLoc l1 _ a) (SrcLoc l2 _ b) = l1 == l2 && a <= b
srcLocEndsBefore _ _ = False

-- | A value with associated source location.
data Located v = Loc {lvSrcLoc :: SrcLoc, locVal :: v}
  deriving (Eq, Ord, Functor, Show, Generic, Data, Hashable, ToJSON, FromJSON)
