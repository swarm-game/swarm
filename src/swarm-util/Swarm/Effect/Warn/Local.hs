-- | Accumulates thread-local warnings.
module Swarm.Effect.Warn.Local (
  Warn,
  warn,
  runWarn,
  evalWarn,
)
where

import Effectful
import Effectful.Labeled
import Swarm.Effect.Output.Local

-- | The Warn effect
type Warn w = Labeled "Warning" (Output w)

-- | Log a single failure as a warning.
warn :: forall w es. (Warn w :> es) => w -> Eff es () -- (Warn w :> es) => w -> Eff es ()
warn w = labeled @"Warning" @(Output w) (output w)

-- | Run the Warn effect, accumulating all warnings
runWarn :: forall w es a. Eff (Warn w : es) a -> Eff es (a, [w])
runWarn = runLabeled @"Warning" @(Output w) runOutput

-- | Run the Warn effect, discarding all warnings
evalWarn :: forall w es a. Eff (Warn w : es) a -> Eff es a
evalWarn = fmap fst . runWarn
