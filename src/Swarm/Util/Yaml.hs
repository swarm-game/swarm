{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Swarm.Util.Yaml
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Various utilities related to parsing YAML files.
module Swarm.Util.Yaml (
  With (..),
  ParserE,
  liftE,
  withE,
  FromJSONE (..),
  decodeFileEitherE,
  (..:),
  (..:?),
  (..!=),
  withTextE,
  withObjectE,
  withArrayE,
) where

import Control.Monad.Reader
import Data.Aeson.Types (explicitParseField, explicitParseFieldMaybe)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Yaml as Y

------------------------------------------------------------
-- WithEntities wrapper
------------------------------------------------------------

-- | A generic wrapper for computations which also depend on knowing a
--   value of type @e@.
newtype With e f a = E {runE :: e -> f a}
  deriving (Functor)
  deriving (Applicative, Monad, MonadFail) via (ReaderT e f)

-- | A 'ParserE' is a YAML 'Parser' that can also depend on knowing an
--   value of type @e@.  The @E@ used to stand for @EntityMap@, but now
--   that it is generalized, it stands for Environment.
type ParserE e = With e Parser

-- | Lift a computation that does not care about the environment
--   value.
liftE :: Functor f => f a -> With e f a
liftE = E . const

withE :: Semigroup e => e -> With e f a -> With e f a
withE e (E f) = E (f . (<> e))

------------------------------------------------------------
-- FromJSONE
------------------------------------------------------------

-- | 'FromJSONE' governs values that can be parsed from a YAML (or
--   JSON) file, but which also have access to an extra, read-only
--   environment value.
--
--   For things that don't care about the environment, the default
--   implementation of 'parseJSONE' simply calls 'parseJSON' from a
--   'FromJSON' instance.
class FromJSONE e a where
  parseJSONE :: Value -> ParserE e a
  default parseJSONE :: FromJSON a => Value -> ParserE e a
  parseJSONE = liftE . parseJSON

  parseJSONE' :: e -> Value -> Parser a
  parseJSONE' e = ($ e) . runE . parseJSONE

instance FromJSONE e Int

instance FromJSONE e a => FromJSONE e [a] where
  parseJSONE = withArrayE "[]" (traverse parseJSONE . V.toList)

instance (FromJSONE e a, FromJSONE e b) => FromJSONE e (a, b) where
  parseJSONE = withArrayE "(a, b)" $ \t ->
    let n = V.length t
     in if n == 2
          then
            (,)
              <$> parseJSONE (V.unsafeIndex t 0)
              <*> parseJSONE (V.unsafeIndex t 1)
          else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 2"

------------------------------------------------------------
-- Decoding
------------------------------------------------------------

-- | Read a value from a YAML file, providing the needed extra
--   environment.
decodeFileEitherE :: FromJSONE e a => e -> FilePath -> IO (Either ParseException a)
decodeFileEitherE e file = do
  res <- decodeFileEither file :: IO (Either ParseException Value)
  return $ case res of
    Left err -> Left err
    Right v -> first AesonException $ parseEither (parseJSONE' e) v

------------------------------------------------------------
-- Accessors
------------------------------------------------------------

-- | A variant of '.:' for 'ParserE': project out a field of an
--   'Object', passing along the extra environment.
(..:) :: FromJSONE e a => Object -> Text -> ParserE e a
v ..: x = E $ \e -> explicitParseField (parseJSONE' e) v x

-- | A variant of '.:?' for 'ParserE': project out an optional field of an
--   'Object', passing along the extra environment.
(..:?) :: FromJSONE e a => Object -> Text -> ParserE e (Maybe a)
v ..:? x = E $ \e -> explicitParseFieldMaybe (parseJSONE' e) v x

-- | A variant of '.!=' for any functor.
(..!=) :: Functor f => f (Maybe a) -> a -> f a
p ..!= a = fromMaybe a <$> p

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

withThingE ::
  (forall b. String -> (thing -> Parser b) -> Value -> Parser b) ->
  (String -> (thing -> ParserE e a) -> Value -> ParserE e a)
withThingE withThing name f = E . (\v es -> withThing name (($ es) . runE . f) v)

-- | @'withTextE' name f value@ applies @f@ to the 'Text' when @value@ is
--   a @String@ and fails otherwise.
withTextE :: String -> (Text -> ParserE e a) -> Value -> ParserE e a
withTextE = withThingE withText

-- | @'withObjectE' name f value@ applies @f@ to the 'Object' when @value@ is
--   an 'Object' and fails otherwise.
withObjectE :: String -> (Object -> ParserE e a) -> Value -> ParserE e a
withObjectE = withThingE withObject

-- | @'withArrayE' name f value@ applies @f@ to the 'Array' when @value@ is
--   an 'Array' and fails otherwise.
withArrayE :: String -> (Y.Array -> ParserE e a) -> Value -> ParserE e a
withArrayE = withThingE withArray
