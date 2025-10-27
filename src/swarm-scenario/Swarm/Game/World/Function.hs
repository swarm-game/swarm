{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Grid on which the game takes place
--
-- Function to produce world with entities and terrain.
module Swarm.Game.World.Function (
  WorldFun (..),
  runWF,
  worldFunFromArray,
) where

import Control.Lens hiding (use)
import Data.Array.IArray
import Data.Bifunctor (second)
import Data.Int (Int32)
import Data.Semigroup (Last (..))
import Swarm.Game.World.Coords
import Swarm.Util.Erasable
import Prelude hiding (Foldable (..), lookup)

-- | A @WorldFun t e@ represents a 2D world with terrain of type @t@
-- (exactly one per cell) and entities of type @e@ (at most one per
-- cell).
newtype WorldFun t e = WF {getWF :: Coords -> (t, Erasable (Last e))}
  deriving stock (Functor)
  deriving newtype (Semigroup, Monoid)

runWF :: WorldFun t e -> Coords -> (t, Maybe e)
runWF wf = second (erasableToMaybe . fmap getLast) . getWF wf

instance Bifunctor WorldFun where
  bimap g h (WF z) = WF (bimap g (fmap (fmap h)) . z)

-- | Create a world function from a finite array of specified cells.
worldFunFromArray :: Monoid t => Array (Int32, Int32) (t, Erasable e) -> WorldFun t e
worldFunFromArray arr = WF $ \(Coords (r, c)) ->
  if inRange bnds (r, c)
    then second (fmap Last) (arr ! (r, c))
    else mempty
 where
  bnds = bounds arr
