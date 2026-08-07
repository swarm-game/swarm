{-# LANGUAGE TypeFamilies #-}

-- | Statically dispatched output effect which enables
-- accumulation of values in a list. When the next version
-- of effectful-core releases, we should migrate to the Output
-- provided by it. The list is thread-local.
-- Note: Code extracted from as-of-yet unreleased
-- implementation of Output effect in effectful-core
module Swarm.Effect.Output.Local (
  -- * Effect
  Output,

  -- ** Handlers
  runOutput,

  -- ** Operations
  output,
) where

import Data.Kind

import Effectful
import Effectful.Dispatch.Static

-- | Provide access to accumulation of values of type @o@ in a thread local
-- list.
data Output (o :: Type) :: Effect

type instance DispatchOf (Output o) = Static NoSideEffects
newtype instance StaticRep (Output o) = Output [o]

-- | Run the 'Output' effect and return the final value along with the
-- accumulated list.
runOutput :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutput action = do
  (a, Output acc) <- runStaticRep (Output []) action
  pure (a, reverse acc)

-- | Append the value to the end of the list.
output ::
  (HasCallStack, Output o :> es) =>
  -- | The value.
  o ->
  Eff es ()
output !o = stateStaticRep $ \(Output acc) -> ((), Output (o : acc))
