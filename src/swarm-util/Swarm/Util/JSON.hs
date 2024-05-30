-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for JSON de/serialization.
module Swarm.Util.JSON where

import Data.Aeson qualified as A

-- | @aeson@ options specifying to unwrap unary records so they are
--   encoded as a simple value instead of as a JSON object.
optionsUnwrapUnary :: A.Options
optionsUnwrapUnary = A.defaultOptions {A.unwrapUnaryRecords = True}
