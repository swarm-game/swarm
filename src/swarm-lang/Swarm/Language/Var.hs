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
import Swarm.Util (showT)
import Prettyprinter (pretty, Doc)

-- | A variable is represented by a textual name as well as a version
--   number which we can use to differentiate between names which are
--   otherwise the same, when one shadows the other.
--
--   See Note [Shadowing for value-level and type-level variables]
data Var = Var { varName :: Text, varVersion :: Int }
  deriving (Eq, Ord, Show, Data, Generic, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

  -- TODO(XXX): make custom To/FromJSON instances to omit the version if it is 0

-- ~~~~ Note [Shadowing for value-level and type-level variables]
--
-- The problem we are solving here is: what happens when y is defined
-- in terms of x, but then later x is redefined/shadowed?  We want to
-- make sure that y still refers to the *old*, shadowed version of x,
-- not the new one.  This problem with user-defined type names caused
-- a type soundness bug, https://github.com/swarm-game/swarm/issues/2437 .
--
-- So how do the version numbers help solve this... and why wasn't it
-- already a problem with value-level names (i.e. things defined via
-- `def`)?
--
-- The language is strict at the value level.  Any use of a variable
-- will be immediately evaluated.  The resulting value will either not
-- mention variables any more, OR will be some kind of closure that
-- contains an environment mapping in-scope variables to their values.
-- So if y is defined in terms of x, it will be evaluated and either
-- not refer to x any more (e.g. `def y = x + 2 end`) or contain a
-- closure with the current value of x.  So if x is later shadowed it
-- makes no difference to the value of y.
--
-- On the other hand, user-defined types are evaluated lazily.  This
-- is a feature: otherwise it would be very annoying to use them for
-- purposes of abbreviating long types (see the discussion at
-- https://github.com/swarm-game/swarm/pull/2438).
--
-- One way this could be solved is by making each type a closure that
-- stores a TDCtx along with it.  However, that would be pretty
-- annoying and require large and tedious changes to the codebase.
-- The other solution is to use version numbers: when a user type is
-- defined, it is given a version number which is one greater than the
-- latest version of the same name already in the TDCtx.  Other
-- definitions referring to older versions will continue to refer to
-- them, and the definitions of the older versions will be
-- retained. When a user type name is used, it is parsed initially as
-- version 0, but in a later name resolution phase (see
-- 'Swarm.Language.Kindcheck.resolveTydefs') the version is updated to
-- the latest in-scope version.

-- | Create a variable with the given name and a default version of 0.
mkVar :: Text -> Var
mkVar = mkVar' 0

-- | Create a variable with a given version number and name.
mkVar' :: Int -> Text -> Var
mkVar' v x = Var x v

-- | Pretty-print a variable, given an extra argument representing the
--   latest version of any variable with this name.  If this variable
--   is the latest version, just print its name.  If it is not the
--   latest version, print @name%version@.
prettyVar :: Int -> Var -> Doc ann
prettyVar latest (Var x n)
  | latest > n = pretty (x <> "%" <> showT n)
  | otherwise  = pretty x

-- TODO(#2452): overhaul pretty-printing to take extra
-- environment/parameters, such as TDCtx to know when to print a user
-- type name with its version number, to disambiguate when it has been
-- shadowed.

-- | The 'PrettyPrec' instance for 'Var' never prints the version
--   number.  If you care about the version number possibly being
--   printed, you must use 'prettyVar' instead.
instance PrettyPrec Var where
  prettyPrec _ (Var x _) = pretty x

instance IsString Var where
  fromString = mkVar . fromString
