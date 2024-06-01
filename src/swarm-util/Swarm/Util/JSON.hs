-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for JSON de/serialization.
module Swarm.Util.JSON where

import Data.Aeson

-- | @aeson@ options specifying to unwrap unary records so they are
--   encoded as a simple value instead of as a JSON object.
optionsUnwrapUnary :: Options
optionsUnwrapUnary = defaultOptions {unwrapUnaryRecords = True}

optionsObjectSingleField :: Options
optionsObjectSingleField = defaultOptions {sumEncoding = ObjectWithSingleField}

optionsUntagged :: Options
optionsUntagged = defaultOptions {sumEncoding = UntaggedValue}
