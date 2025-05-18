{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Variables.
module Swarm.Language.Var where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Data (Data)
import Data.Hashable
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Pretty (PrettyPrec (..))
import Prettyprinter (pretty)

-- | A variable is represented by a textual name as well as a "version
--   number" which we use to differentiate between names which are
--   otherwise the same, when one shadows the other.
data Var = Var { varName :: Text, varVersion :: Int }
  deriving (Eq, Ord, Show, Data, Generic, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  -- XXX make custom instances to omit the version if it is 0

-- | XXX
mkVar :: Text -> Var
mkVar x = Var x 0

instance PrettyPrec Var where
  prettyPrec _ (Var x _) = pretty x    -- XXX print version sometimes

instance IsString Var where
  fromString = mkVar . fromString
