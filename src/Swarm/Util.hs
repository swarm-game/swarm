{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------

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
module Swarm.Util (
  -- * Miscellaneous utilities
  (?),
  maxOn,
  maximum0,
  readFileMay,
  cycleEnum,

  -- * English language utilities
  quote,
  squote,
  commaList,
  indefinite,
  indefiniteQ,
  plural,
  number,

  -- * Validation utilities
  holdsOr,
  isJustOr,
  isRightOr,
  isSuccessOr,

  -- * Template Haskell utilities
  liftText,

  -- * Fused-Effects Lens utilities
  (%%=),
  (<%=),
  (<+=),
  (<<.=),
  (<>=),
) where

import Control.Algebra (Has)
import Control.Effect.State (State, modify, state)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens (ASetter', LensLike, LensLike', Over, (<>~))
import Control.Monad (unless)
import Data.Either.Validation
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Yaml
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Linear (V2)
import qualified NLP.Minimorph.English as MM
import NLP.Minimorph.Util ((<+>))
import System.Directory (doesFileExist)

infixr 1 ?
infix 4 %%=, <+=, <%=, <<.=, <>=

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

-- | Find the maximum of a list of numbers, defaulting to 0 if the
--   list is empty.
maximum0 :: (Num a, Ord a) => [a] -> a
maximum0 [] = 0
maximum0 xs = maximum xs

-- | Safely attempt to read a file, returning @Nothing@ if the file
--   does not exist.  \"Safely\" should be read in scare quotes here,
--   since /e.g./ we do nothing to guard against the possibility of a
--   race condition where the file is deleted after the existence
--   check but before trying to read it.  But it's not like we're
--   worried about security or anything here.
readFileMay :: FilePath -> IO (Maybe String)
readFileMay file = do
  b <- doesFileExist file
  case b of
    False -> return Nothing
    True -> Just <$> readFile file

-- | Take the successor of an 'Enum' type, wrapping around when it
--   reaches the end.
cycleEnum :: (Eq e, Enum e, Bounded e) => e -> e
cycleEnum e
  | e == maxBound = minBound
  | otherwise = succ e

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

-- | Either pluralize a noun or not, depending on the value of the
--   number.
number :: Int -> Text -> Text
number 1 = id
number _ = plural

-- | Surround some text in single quotes.
squote :: Text -> Text
squote t = T.concat ["'", t, "'"]

-- | Surround some text in double quotes.
quote :: Text -> Text
quote t = T.concat ["\"", t, "\""]

-- | Make a list of things with commas and the word "and".
commaList :: [Text] -> Text
commaList [] = ""
commaList [t] = t
commaList [s, t] = T.unwords [s, "and", t]
commaList ts = T.unwords $ map (`T.append` ",") (init ts) ++ ["and", last ts]

------------------------------------------------------------
-- Some orphan instances

deriving instance ToJSON (V2 Int64)
deriving instance FromJSON (V2 Int64)

------------------------------------------------------------
-- Validation utilities

-- | Require that a Boolean value is @True@, or throw an exception.
holdsOr :: Has (Throw e) sig m => Bool -> e -> m ()
holdsOr b e = unless b $ throwError e

-- | Require that a 'Maybe' value is 'Just', or throw an exception.
isJustOr :: Has (Throw e) sig m => Maybe a -> e -> m a
Just a `isJustOr` _ = return a
Nothing `isJustOr` e = throwError e

-- | Require that an 'Either' value is 'Right', or throw an exception
--   based on the value in the 'Left'.
isRightOr :: Has (Throw e) sig m => Either b a -> (b -> e) -> m a
Right a `isRightOr` _ = return a
Left b `isRightOr` f = throwError (f b)

-- | Require that a 'Validation' value is 'Success', or throw an exception
--   based on the value in the 'Failure'.
isSuccessOr :: Has (Throw e) sig m => Validation b a -> (b -> e) -> m a
Success a `isSuccessOr` _ = return a
Failure b `isSuccessOr` f = throwError (f b)

------------------------------------------------------------
-- Template Haskell utilities

-- See https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

------------------------------------------------------------
-- Fused-Effects Lens utilities

(<+=) :: (Has (State s) sig m, Num a) => LensLike' ((,) a) s a -> a -> m a
l <+= a = l <%= (+ a)
{-# INLINE (<+=) #-}

(<%=) :: (Has (State s) sig m) => LensLike' ((,) a) s a -> (a -> a) -> m a
l <%= f = l %%= (\b -> (b, b)) . f
{-# INLINE (<%=) #-}

(%%=) :: (Has (State s) sig m) => Over p ((,) r) s s a b -> p a (r, b) -> m r
l %%= f = state (swap . l f)
{-# INLINE (%%=) #-}

(<<.=) :: (Has (State s) sig m) => LensLike ((,) a) s s a b -> b -> m a
l <<.= b = l %%= \a -> (a, b)
{-# INLINE (<<.=) #-}

(<>=) :: (Has (State s) sig m, Semigroup a) => ASetter' s a -> a -> m ()
l <>= a = modify (l <>~ a)
{-# INLINE (<>=) #-}
