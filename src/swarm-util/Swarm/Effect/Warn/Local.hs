{-# LANGUAGE TypeFamilies #-}

-- | Accumulates thread-local warnings.
-- Code adapted from the as-of-yet unreleased Output effect in
-- the effectful repo
module Swarm.Effect.Warn.Local (
  Warn,
  warn,
  runWarn,
  evalWarn,
)
where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Static

-- | The Warn effect
data Warn (w :: Type) :: Effect

type instance DispatchOf (Warn w) = Static NoSideEffects
newtype instance StaticRep (Warn w) = Warn [w]

-- | Log a single failure as a warning.
warn :: forall w es. (Warn w :> es) => w -> Eff es () -- (Warn w :> es) => w -> Eff es ()
warn !w = stateStaticRep $ \(Warn ws) -> ((), Warn (w : ws))

-- | Run the Warn effect, accumulating all warnings
runWarn :: forall w es a. Eff (Warn w : es) a -> Eff es (a, [w])
runWarn action = do
  (a, Warn warnings) <- runStaticRep (Warn []) action
  pure (a, reverse warnings)

-- | Run the Warn effect, discarding all warnings
-- Note that you need to call this function as @evalWarn \@w@
-- to avoid an ambiguous type error.
evalWarn :: forall w es a. Eff (Warn w : es) a -> Eff es a
evalWarn = fmap fst . runWarn
