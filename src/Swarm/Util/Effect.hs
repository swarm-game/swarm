-- XXX

module Swarm.Util.Effect where

import Control.Carrier.Error.Either (ErrorC (..))
import Control.Carrier.Throw.Either (ThrowC (..), runThrow)
import Control.Effect.Accum
import Control.Effect.Throw
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (partitionEithers)
import Data.Either.Extra (eitherToMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

-- | Transform a @Throw e1@ constraint into a @Throw e2@ constraint,
--   by supplying an adapter function of type @(e1 -> e2)@.
withThrow :: (Has (Throw e2) sig m) => (e1 -> e2) -> ThrowC e1 m a -> m a
withThrow f = runThrow >=> either (throwError . f) return

-- | Transform a @Throw e@ constrint into a concrete @Maybe@,
--   discarding the error.
throwToMaybe :: forall e m a. Functor m => ThrowC e m a -> m (Maybe a)
throwToMaybe = fmap eitherToMaybe . runThrow

-- | Convert a fused-effects style computation using a @Throw e@
--   constraint into an @ExceptT@ computation.  This is mostly a stub
--   to convert from one style to the other while we are in the middle
--   of incrementally converting.  Eventually this should not be needed.
asExceptT :: ThrowC e m a -> ExceptT e m a
asExceptT (ThrowC (ErrorC m)) = m

-- | A version of 'forM' that can also accumulate warnings.
forMW ::
  Has (Accum (Seq w)) sig m =>
  [a] ->
  (a -> m (Either w b)) ->
  m [b]
forMW as f = do
  (ws, bs) <- partitionEithers <$> mapM f as
  add (Seq.fromList ws)
  return bs
