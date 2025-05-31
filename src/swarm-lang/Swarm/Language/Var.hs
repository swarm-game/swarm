-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Value-level variables.
module Swarm.Language.Var where

import Data.Text (Text)

-- | For now, we just use 'Text' to represent variables.  In theory,
--   at some point in the future we might want to represent them in some
--   fancier way.
type Var = Text
