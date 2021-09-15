{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Util
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A random collection of small, useful functions that are (or could
-- be) used throughout the code base.
--
-----------------------------------------------------------------------------

module Swarm.Util where

import           Control.Monad             (unless)
import           Control.Monad.Error.Class
import           Data.Either.Validation
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Yaml
import           Linear                    (V2)
import qualified NLP.Minimorph.English     as MM
import           NLP.Minimorph.Util        ((<+>))
import           System.Directory          (doesFileExist)


infixr 1 ?

-- | A convenient infix flipped version of 'fromMaybe': @Just a ? b =
--   a@, and @Nothing ? b = b@. It can also be chained, as in @x ? y ?
--   z ? def@, which takes the value inside the first @Just@,
--   defaulting to @def@ as a last resort.
(?) :: Maybe a -> a -> a
(?) = flip fromMaybe

-- | Find the maximum of two values, comparing them according to a
--   custom projection function.
maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y
  | f x > f y = x
  | otherwise = y

-- | Safely attempt to read a file, returning @Nothing@ if the file
--   does not exist.  \"Safely\" should be read in scare quotes here,
--   since /e.g./ we do nothing to guard against the possibility of a race
--   condition where the file is deleted after the existence check but
--   before trying to read it.  But seriously, who does that?
readFileMay :: FilePath -> IO (Maybe String)
readFileMay file = do
  b <- doesFileExist file
  case b of
    False -> return Nothing
    True  -> Just <$> readFile file

--------------------------------------------------
-- Some language-y stuff

-- | Prepend a noun with the proper indefinite article (\"a\" or \"an\").
indefinite :: Text -> Text
indefinite w = MM.indefiniteDet w <+> w

-- | Prepend a noun with the proper indefinite article, and surround
--   the noun in single quotes.
indefiniteQ :: Text -> Text
indefiniteQ w = MM.indefiniteDet w <+> squote w

-- | Pluralize a noun.
plural :: Text -> Text
plural = MM.defaultNounPlural
  -- For now, it is just MM.defaultNounPlural, which only uses heuristics;
  -- in the future, if we discover specific nouns that it gets wrong,
  -- we can add a lookup table.

-- | XXX
number :: Int -> Text -> Text
number 1 = id
number _ = plural

-- | Surround some text in single quotes
squote :: Text -> Text
squote t = T.concat ["'", t, "'"]

-- | Surround some text in double quotes.
quote :: Text -> Text
quote t = T.concat ["\"", t, "\""]

------------------------------------------------------------
-- Some orphan instances

deriving instance ToJSON (V2 Int)
deriving instance FromJSON (V2 Int)

------------------------------------------------------------
-- Validation utilities

-- | Require that a Boolean value is @True@, or throw an exception.
holdsOr :: MonadError e m => Bool -> e -> m ()
holdsOr b e = unless b $ throwError e

-- | Require that a 'Maybe' value is 'Just', or throw an exception.
isJustOr :: MonadError e m => Maybe a -> e -> m a
Just a  `isJustOr` _ = return a
Nothing `isJustOr` e = throwError e

-- | Require that an 'Either' value is 'Right', or throw an exception
--   based on the value in the 'Left'.
isRightOr :: MonadError e m => Either b a -> (b -> e) -> m a
Right a `isRightOr` _ = return a
Left b `isRightOr` f  = throwError (f b)

-- | Require that a 'Validation' value is 'Success', or throw an exception
--   based on the value in the 'Failure'.
isSuccessOr :: MonadError e m => Validation b a -> (b -> e) -> m a
Success a `isSuccessOr` _ = return a
Failure b `isSuccessOr` f = throwError (f b)
