{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for working with locations of something in source code.
module Swarm.Language.Syntax.Loc (
  SrcLoc (..),
  LocVar (..),
  srcLocStartsBefore,
  srcLocEndsBefore,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Swarm.Language.Context (Var)
import Swarm.Util.JSON (optionsUntagged)

------------------------------------------------------------
-- SrcLoc
------------------------------------------------------------

-- | The location of something in the textual source code (recorded as
--   an interval measured in terms of indices into the input stream).
data SrcLoc
  = NoLoc
  | -- | Half-open interval from start (inclusive) to end (exclusive)
    SrcLoc Int Int
  deriving (Eq, Ord, Show, Data, Generic, Hashable)

instance ToJSON SrcLoc where
  toJSON = genericToJSON optionsUntagged

instance FromJSON SrcLoc where
  parseJSON = genericParseJSON optionsUntagged

-- | @x <> y@ is the smallest 'SrcLoc' that subsumes both @x@ and @y@.
instance Semigroup SrcLoc where
  NoLoc <> l = l
  l <> NoLoc = l
  SrcLoc s1 e1 <> SrcLoc s2 e2 = SrcLoc (min s1 s2) (max e1 e2)

-- | @mempty@ is a special value which means we have no location
--   information.
instance Monoid SrcLoc where
  mempty = NoLoc

-- | Check whether one @SrcLoc@ starts before another one,
--   /i.e./ compare their starting indices to see if the first is @<=@
--   the second.
srcLocStartsBefore :: SrcLoc -> SrcLoc -> Bool
srcLocStartsBefore (SrcLoc a _) (SrcLoc b _) = a <= b
srcLocStartsBefore _ _ = False

-- | Check whether the first @SrcLoc@ ends before the second, /i.e./
--   compare their ending indices to see if the first is @<=@ the
--   second.
srcLocEndsBefore :: SrcLoc -> SrcLoc -> Bool
srcLocEndsBefore (SrcLoc _ a) (SrcLoc _ b) = a <= b
srcLocEndsBefore _ _ = False

-- | A variable with associated source location, used for variable
--   binding sites. (Variable occurrences are a bare TVar which gets
--   wrapped in a Syntax node, so we don't need LocVar for those.)
data LocVar = LV {lvSrcLoc :: SrcLoc, lvVar :: Var}
  deriving (Eq, Ord, Show, Data, Generic, Hashable, FromJSON, ToJSON)
