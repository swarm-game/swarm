-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Value-level variables.
module Swarm.Language.Var where

import Data.Text (Text)
import Swarm.Util.SrcLoc (Located)

-- | For now, we just use 'Text' to represent variables.  In theory,
--   at some point in the future we might want to represent them in some
--   fancier way.
type Var = Text

-- | A variable with associated source location, used for variable
--   binding sites. (Variable occurrences are a bare TVar which gets
--   wrapped in a Syntax node, so we don't need LocVar for those.)
type LocVar = Located Var
