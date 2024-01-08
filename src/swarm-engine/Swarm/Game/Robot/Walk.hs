{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Walkability exceptions
module Swarm.Game.Robot.Walk where

import Control.Monad (unless)
import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Generics (Generic)
import Swarm.Game.Entity (EntityName)
import Swarm.Language.Capability (Capability)

data Inclusions a
  = Whitelist a
  | Blacklist a
  deriving (Show, Eq, Functor, Generic, ToJSON)

emptyExceptions :: Monoid a => Inclusions a
emptyExceptions = Blacklist mempty

type WalkabilityExceptions a = Inclusions (Set a)

instance (FromJSON a, Ord a) => FromJSON (WalkabilityExceptions a) where
  parseJSON = withObject "walkable" $ \v -> do
    whitelist <- v .:? "only" .!= []
    blacklist <- v .:? "never" .!= []

    unless (null whitelist || null blacklist) $
      fail "Cannot specify both a whitelist and blacklist"

    let exceptionList =
          maybe
            (Blacklist blacklist)
            (Whitelist . NE.toList)
            (NE.nonEmpty whitelist)

    return $ S.fromList <$> exceptionList

-- | Properties of a robot used to determine whether an entity is walkable
data WalkabilityContext
  = WalkabilityContext
      (Set Capability)
      -- | which entities are unwalkable by this robot
      (WalkabilityExceptions EntityName)
  deriving (Show, Eq, Generic, ToJSON)
