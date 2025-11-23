{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Various utilities related to parsing YAML files.
module Swarm.Util.Yaml (
  With (..),
  ParserE,
  liftE,
  localE,
  withE,
  getE,
  getProvenance,
  FromJSONE (..),
  decodeFileEitherE,
  (..:),
  (..:?),
  (..!=),
  withTextE,
  withObjectE,
  withArrayE,
) where

import Control.Applicative (Alternative)
import Control.Monad.Reader
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (explicitParseField, explicitParseFieldMaybe)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Yaml as Y
import Swarm.Util (failT, showT)

------------------------------------------------------------
-- With wrapper
------------------------------------------------------------

-- | A generic wrapper for computations which also depend on knowing a
--   value of type @e@, and possibly the provenance of the information.
newtype With e (f :: * -> *) a = E {runE :: e -> Maybe FilePath -> f a}
  deriving (Functor)
  deriving (Applicative, Monad, MonadFail, Alternative) via (ReaderT e (ReaderT (Maybe FilePath) f))

-- | A 'ParserE' is a YAML 'Parser' that can also depend on knowing an
--   value of type @e@, as well as the provenance of the yaml being
--   parsed.  The @E@ used to stand for @EntityMap@, but now that it
--   is generalized, it stands for Environment.
type ParserE e = With e Parser

-- | Lift a computation that does not care about the environment
--   value or provenance.
liftE :: Functor f => f a -> With e f a
liftE = E . const . const

-- | Locally modify an environment.
localE :: (e' -> e) -> With e f a -> With e' f a
localE g (E f) = E (f . g)

-- | Locally merge an environment with the current one for given action.
withE :: Semigroup e => e -> With e f a -> With e f a
withE e = localE (<> e)

-- | Get the current environment.
getE :: Applicative f => With e f e
getE = E (const . pure)

getProvenance :: Applicative f => With e f (Maybe FilePath)
getProvenance = E (const pure)

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

  parseJSONE' :: Maybe FilePath -> e -> Value -> Parser a
  parseJSONE' p e = ($ p) . ($ e) . runE . parseJSONE

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
          else failT ["cannot unpack array of length", showT n, "into a tuple of length 2"]

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
    Right v -> first AesonException $ parseEither (parseJSONE' (Just file) e) v

------------------------------------------------------------
-- Accessors
------------------------------------------------------------

-- | A variant of '.:' for 'ParserE': project out a field of an
--   'Object', passing along the extra environment.
(..:) :: FromJSONE e a => Object -> Text -> ParserE e a
v ..: x = E $ \e p -> explicitParseField (parseJSONE' p e) v (fromText x)

-- | A variant of '.:?' for 'ParserE': project out an optional field of an
--   'Object', passing along the extra environment.
(..:?) :: FromJSONE e a => Object -> Text -> ParserE e (Maybe a)
v ..:? x = E $ \e p -> explicitParseFieldMaybe (parseJSONE' p e) v (fromText x)

-- | A variant of '.!=' for any functor.
(..!=) :: Functor f => f (Maybe a) -> a -> f a
p ..!= a = fromMaybe a <$> p

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

withThingE ::
  (forall b. String -> (thing -> Parser b) -> Value -> Parser b) ->
  (String -> (thing -> ParserE e a) -> Value -> ParserE e a)
withThingE withThing name f = E . (\v es p -> withThing name (($ p) . ($ es) . runE . f) v)

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
