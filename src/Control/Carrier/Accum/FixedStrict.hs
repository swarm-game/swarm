-- This file is a temporary copy of the code from fused-effects, with
-- https://github.com/fused-effects/fused-effects/issues/449 fixed
-- (the fixed line of code is marked with a comment below).  We should
-- keep this only until the above issue is fixed upstream.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for 'Accum' effects.
-- This carrier performs its append operations strictly and thus avoids the space leaks inherent in lazy writer monads.
-- These appends are left-associative; as such, @[]@ is a poor choice of monoid for computations that entail many calls to 'tell'.
-- The [Seq](http://hackage.haskell.org/package/containersdocs/Data-Sequence.html) or [DList](http://hackage.haskell.org/package/dlist) monoids may be a superior choice.
--
-- @since 1.1.2.0
module Control.Carrier.Accum.FixedStrict (
  -- * Accum carrier
  runAccum,
  execAccum,
  evalAccum,
  AccumC (AccumC),

  -- * Accum effect
  module Control.Effect.Accum,
) where

import Control.Algebra
import Control.Applicative (Alternative (..))
import Control.Effect.Accum
import Control.Monad (MonadPlus (..))
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run an 'Accum' effect with a 'Monoid'al log, applying a continuation to the final log and result.
--
-- @
-- 'runAccum' w0 ('pure' a) = 'pure' (w0, a)
-- @
-- @
-- 'runAccum' w0 ('add' w) = 'pure' (w0 <> w, ())
-- @
-- @
-- 'runAccum' w0 ('add' w >> 'look') = 'pure' (w0 <> w, w0 <> w)
-- @
--
-- @since 1.1.2.0
runAccum :: w -> AccumC w m a -> m (w, a)
runAccum = flip runAccumC
{-# INLINE runAccum #-}

-- | Run a 'Accum' effect (typically with a 'Monoid'al log),
--   producing the final log and discarding the result value.
--
-- @
-- 'execAccum' w = 'fmap' 'fst' . 'runAccum' w
-- @
--
-- @since 1.1.2.0
execAccum :: (Functor m) => w -> AccumC w m a -> m w
execAccum w = fmap fst . runAccum w
{-# INLINE execAccum #-}

-- | Run a 'Accum' effect (typically with a 'Monoid'al log),
--   producing the result value and discarding the final log.
--
-- @
-- 'evalAccum' w = 'fmap' 'snd' . 'runAccum' w
-- @
--
-- @since 1.1.2.0
evalAccum :: (Functor m) => w -> AccumC w m a -> m a
evalAccum w = fmap snd . runAccum w
{-# INLINE evalAccum #-}

-- | @since 1.1.2.0
newtype AccumC w m a = AccumC {runAccumC :: w -> m (w, a)}

instance Monoid w => MonadTrans (AccumC w) where
  lift ma = AccumC $ \_ -> (mempty,) <$> ma
  {-# INLINE lift #-}

instance Functor m => Functor (AccumC w m) where
  fmap f ma = AccumC $ fmap (fmap f) . runAccumC ma
  {-# INLINE fmap #-}

instance (Monad m, Monoid w) => Applicative (AccumC w m) where
  pure a = AccumC $ const $ pure (mempty, a)
  {-# INLINE pure #-}

  mf <*> ma = AccumC $ \w -> do
    (w', f) <- runAccumC mf w
    (w'', a) <- runAccumC ma $ mappend w w'
    return (mappend w' w'', f a)
  {-# INLINE (<*>) #-}

instance (Alternative m, Monad m, Monoid w) => Alternative (AccumC w m) where
  empty = lift empty
  {-# INLINE empty #-}

  ma1 <|> ma2 = AccumC $ \w -> runAccumC ma1 w <|> runAccumC ma2 w
  {-# INLINE (<|>) #-}

instance (Monad m, Monoid w) => Monad (AccumC w m) where
  ma >>= f = AccumC $ \w -> do
    (w', a) <- runAccumC ma w
    (w'', b) <- runAccumC (f a) $ mappend w w'
    return (mappend w' w'', b)
  {-# INLINE (>>=) #-}

instance (MonadPlus m, Monoid w) => MonadPlus (AccumC w m) where
  mzero = lift mzero
  {-# INLINE mzero #-}

  ma1 `mplus` ma2 = AccumC $ \w -> runAccumC ma1 w `mplus` runAccumC ma2 w
  {-# INLINE mplus #-}

instance (MonadFail m, Monoid w) => MonadFail (AccumC w m) where
  fail = AccumC . const . Fail.fail
  {-# INLINE fail #-}

instance (MonadFix m, Monoid w) => MonadFix (AccumC w m) where
  mfix ma = AccumC $ \w -> mfix $ flip runAccumC w . ma . snd
  {-# INLINE mfix #-}

instance (MonadIO m, Monoid w) => MonadIO (AccumC w m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (Algebra sig m, Monoid w) => Algebra (Accum w :+: sig) (AccumC w m) where
  alg hdl sig ctx = AccumC $ \w -> case sig of
    L accum -> case accum of
      Add w' -> pure (w', ctx)
      Look -> pure (mempty, w <$ ctx)
    R other -> thread (uncurry runAccum ~<~ hdl) other (mempty, ctx) -- THIS IS THE FIXED LINE
  {-# INLINE alg #-}
