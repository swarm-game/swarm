-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- effectful utilities for Swarm.
module Swarm.Util.Effect where

import Control.Monad ((>=>))
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Either.Extra (eitherToMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Effectful
import Effectful.Error.Static
import Swarm.Effect.Accum.Local
import Witherable

-- | Transform an @Error e1@ constraint into a @Error e2@ constraint,
--   by supplying an adapter function of type @(e1 -> e2)@.
withError :: (HasCallStack, Error e' :> es) => (e -> e') -> Eff (Error e : es) a -> Eff es a
withError f = runErrorNoCallStackWith (throwError_ . f)

-- | Transform a @Throw e@ constraint into a concrete @Maybe@,
--   discarding the error.
errorToMaybe :: (HasCallStack) => Eff (Error e : es) a -> Eff es (Maybe a)
errorToMaybe = fmap eitherToMaybe . runErrorNoCallStack

liftEither :: (HasCallStack, Error e :> es) => Either e a -> Eff es a
liftEither = either throwError_ pure

-- | Transform a @Throw e@ constraint into a concrete @Maybe@,
--   logging any error as a warning.
throwToWarning :: (Accum (Seq e) :> es) => Eff (Error e : es) a -> Eff es (Maybe a)
throwToWarning m = do
  res <- runErrorNoCallStack m
  case res of
    Left err -> warn err >> return Nothing
    Right a -> return (Just a)

-- | Run a computation with an @Accum@ effect (typically accumulating
--   a list of warnings), ignoring the accumulated value.
ignoreWarnings :: (Monoid e) => Eff (Accum e : es) a -> Eff es a
ignoreWarnings = evalAccum mempty

-- | Convert a effectful style computation using an @Error e@
--   constraint into an @ExceptT@ computation.  This is mostly a stub
--   to convert from one style to the other while we are in the middle
--   of incrementally converting.  Eventually this should not be needed.
asExceptT :: Eff [Error e, IOE] a -> ExceptT e IO a
asExceptT = ExceptT . (runEff . runErrorNoCallStack)

-- | Log a single failure as a warning.
warn :: (Accum (Seq w) :> es) => w -> Eff es ()
warn = add . Seq.singleton

-- | A version of 'traverse'/'mapM' that also accumulates warnings.
--
--   Note that we can't generalize this to work over any 'Traversable'
--   because it also needs to have a notion of "filtering".
--   'Witherable' provides exactly the right abstraction.
traverseW ::
  (Accum (Seq w) :> es, Witherable t) =>
  (a -> Eff es (Either w b)) ->
  t a ->
  Eff es (t b)
traverseW f = do
  wither $
    f >=> \case
      Left e -> warn e >> return Nothing
      Right e -> return $ Just e

-- | Flipped version of 'traverseW' for convenience.
forMW ::
  (Accum (Seq w) :> es, Witherable t) =>
  t a ->
  (a -> Eff es (Either w b)) ->
  Eff es (t b)
forMW = flip traverseW

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = get >>= (f >=> put)

infixr 1 ???

-- | Handle an action producing a Maybe by specifying an alternative
--   action in the Nothing case.
(???) :: Monad m => m (Maybe a) -> m a -> m a
m ??? z = m >>= maybe z pure
