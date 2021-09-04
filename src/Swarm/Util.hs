{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

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

import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified NLP.Minimorph.English as MM
import           NLP.Minimorph.Util    ((<+>))
import           System.Directory      (doesFileExist)


infixr 1 ?

-- | Apply a function to all the elements in the tail of a list.
onTail :: (a -> a) -> [a] -> [a]
onTail _ []     = []
onTail f (x:xs) = x : map f xs

-- | A convenient infix flipped version of 'fromMaybe': @Just a ? b =
--   a@, and @Nothing ? b = b@. It can also be chained, as in @x ? y ?
--   z ? def@, which takes the value inside the first @Just@,
--   defaulting to @def@ as a last resort.
(?) :: Maybe a -> a -> a
(?) = flip fromMaybe

-- | A generic type for pairs, using infix syntax reminiscent of the
--   "has type" relation. In this codebase it is often used to package
--   together a term and its type.
data a ::: b = a ::: b

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
--   the noun in double quotes.
indefiniteQ :: Text -> Text
indefiniteQ w = MM.indefiniteDet w <+> quote w

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

-- | Surround some text in double quotes.
quote :: Text -> Text
quote t = T.concat ["\"", t, "\""]
