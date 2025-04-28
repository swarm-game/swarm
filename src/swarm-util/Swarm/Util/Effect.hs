-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- fused-effect utilities for Swarm.
module Swarm.Util.Effect where

import Control.Algebra
import Control.Carrier.Accum.Strict
import Control.Carrier.Error.Either (ErrorC (..))
import Control.Carrier.Throw.Either (ThrowC (..), runThrow)
import Control.Effect.Throw
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT)
import Data.Either.Extra (eitherToMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Witherable

-- | Transform a @Throw e1@ constraint into a @Throw e2@ constraint,
--   by supplying an adapter function of type @(e1 -> e2)@.
withThrow :: (Has (Throw e2) sig m) => (e1 -> e2) -> ThrowC e1 m a -> m a
withThrow f = runThrow >=> either (throwError . f) return

-- | Transform a @Throw e@ constrint into a concrete @Maybe@,
--   discarding the error.
throwToMaybe :: forall e m a. Functor m => ThrowC e m a -> m (Maybe a)
throwToMaybe = fmap eitherToMaybe . runThrow

-- | Transform a @Throw e@ constrint into a concrete @Maybe@,
--   logging any error as a warning.
throwToWarning :: (Has (Accum (Seq e)) sig m) => ThrowC e m a -> m (Maybe a)
throwToWarning m = do
  res <- runThrow m
  case res of
    Left err -> warn err >> return Nothing
    Right a -> return (Just a)

-- | Run a computation with an @Accum@ effect (typically accumulating
--   a list of warnings), ignoring the accumulated value.
ignoreWarnings :: forall e m a. (Monoid e, Functor m) => AccumC e m a -> m a
ignoreWarnings = evalAccum mempty

-- | Convert a fused-effects style computation using a @Throw e@
--   constraint into an @ExceptT@ computation.  This is mostly a stub
--   to convert from one style to the other while we are in the middle
--   of incrementally converting.  Eventually this should not be needed.
asExceptT :: ThrowC e m a -> ExceptT e m a
asExceptT (ThrowC (ErrorC m)) = m

-- | Log a single failure as a warning.
warn :: Has (Accum (Seq w)) sig m => w -> m ()
warn = add . Seq.singleton

-- | A version of 'traverse'/'mapM' that also accumulates warnings.
--
--   Note that we can't generalize this to work over any 'Traversable'
--   because it also needs to have a notion of "filtering".
--   'Witherable' provides exactly the right abstraction.
traverseW ::
  (Has (Accum (Seq w)) sig m, Witherable t) =>
  (a -> m (Either w b)) ->
  t a ->
  m (t b)
traverseW f = do
  wither $
    f >=> \case
      Left e -> warn e >> return Nothing
      Right e -> return $ Just e

-- | Flipped version of 'traverseW' for convenience.
forMW ::
  (Has (Accum (Seq w)) sig m, Witherable t) =>
  t a ->
  (a -> m (Either w b)) ->
  m (t b)
forMW = flip traverseW
