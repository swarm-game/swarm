-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Custom extension of Semigroup -> Monoid that adds an identity + annihilator.
module Swarm.Util.Erasable where

-- | Extend a semigroup to a monoid by adding an identity ('ENothing') /and/ an
--   annihilator ('EErase').
data Erasable e = ENothing | EErase | EJust e
  deriving (Show, Eq, Ord, Functor)

instance Semigroup e => Semigroup (Erasable e) where
  ENothing <> e = e
  e <> ENothing = e
  EErase <> _ = EErase
  _ <> EErase = EErase
  EJust e1 <> EJust e2 = EJust (e1 <> e2)

instance Semigroup e => Monoid (Erasable e) where
  mempty = ENothing

-- | Convert an 'Erasable' value to 'Maybe', turning both 'ENothing'
--   and 'EErase' into 'Nothing'.
erasableToMaybe :: Erasable e -> Maybe e
erasableToMaybe (EJust e) = Just e
erasableToMaybe _ = Nothing

-- | Inject a 'Maybe' value into 'Erasable'.
maybeToErasable :: Maybe e -> Erasable e
maybeToErasable Nothing = ENothing
maybeToErasable (Just e) = EJust e
