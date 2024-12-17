{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Try to read text into a value of a given type.  Left
-- inverse to 'format'.
module Swarm.Game.Step.Read (evalRead) where

import Control.Monad (guard)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Value (asValue)
import Swarm.Language.Types
import Swarm.Language.Value
import Text.Read (readMaybe)

readValueMaybe :: Read a => Text -> Maybe a
readValueMaybe = readMaybe . T.unpack

evalRead :: Type -> Text -> Maybe Value
evalRead ty txt = case ty of
  TyVoid -> Nothing
  TyUnit -> guard (txt == "()") *> pure VUnit
  TyInt -> asValue <$> readValueMaybe @Integer txt
  TyText -> asValue <$> readValueMaybe @Text txt
  _ -> Nothing  -- XXX
