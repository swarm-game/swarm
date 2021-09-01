{-# LANGUAGE TypeOperators #-}

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

import           Data.Maybe       (fromMaybe)
import           System.Directory (doesFileExist)

infixr 1 ?

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
