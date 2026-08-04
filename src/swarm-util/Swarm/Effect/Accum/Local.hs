{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Accum effect. The accumulator is thread-local.
module Swarm.Effect.Accum.Local where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Static

data Accum (w :: Type) :: Effect
type instance DispatchOf (Accum w) = Static NoSideEffects
newtype instance StaticRep (Accum w) = Accum w

runAccum :: (HasCallStack, Monoid w) => w -> Eff (Accum w : es) a -> Eff es (w, a)
runAccum w0 m = do
  (a, Accum w) <- runStaticRep (Accum w0) m
  pure (w, a)

evalAccum :: Monoid w => w -> Eff (Accum w : es) a -> Eff es a
evalAccum w0 m = fmap snd (runAccum w0 m)

add :: (HasCallStack, Accum w :> es, Monoid w) => w -> Eff es ()
add w = stateStaticRep $ \(Accum w0) -> ((), Accum (w0 <> w))

look :: (HasCallStack, Accum w :> es, Monoid w) => Eff es w
look = do
  Accum w <- getStaticRep
  pure w

looks :: (HasCallStack, Accum w :> es, Monoid w) => (w -> a) -> Eff es a
looks f = f <$> look
