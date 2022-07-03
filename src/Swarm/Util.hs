{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  cycleEnum,

  -- * Directory utilities
  readFileMay,
  readFileMayT,
  getSwarmDataPath,
  getSwarmHistoryPath,
  readAppData,

  -- * Text utilities
  isIdentChar,
  replaceLast,

  -- * English language utilities
  reflow,
  quote,
  squote,
  commaList,
  indefinite,
  indefiniteQ,
  singularSubjectVerb,
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
import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Control.Lens (ASetter', LensLike, LensLike', Over, (<>~))
import Control.Monad (forM, unless, when)
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Char (isAlphaNum)
import Data.Either.Validation
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, toUpper)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple (swap)
import Data.Yaml
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Linear (V2)
import NLP.Minimorph.English qualified as MM
import NLP.Minimorph.Util ((<+>))
import Paths_swarm (getDataDir)
import System.Clock (TimeSpec)
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  getXdgDirectory,
  listDirectory,
 )
import System.FilePath
import System.IO
import System.IO.Error (catchIOError)
import Witch

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

-- | Take the successor of an 'Enum' type, wrapping around when it
--   reaches the end.
cycleEnum :: (Eq e, Enum e, Bounded e) => e -> e
cycleEnum e
  | e == maxBound = minBound
  | otherwise = succ e

------------------------------------------------------------
-- Directory stuff

-- | Safely attempt to read a file.
readFileMay :: FilePath -> IO (Maybe String)
readFileMay = catchIO . readFile

-- | Safely attempt to (efficiently) read a file.
readFileMayT :: FilePath -> IO (Maybe Text)
readFileMayT = catchIO . T.readFile

-- | Turns any IO error into Nothing.
catchIO :: IO a -> IO (Maybe a)
catchIO act = (Just <$> act) `catchIOError` (\_ -> return Nothing)

-- | Get path to swarm data, optionally creating necessary
--   directories.
getSwarmDataPath :: Bool -> IO FilePath
getSwarmDataPath createDirs = do
  swarmData <- getXdgDirectory XdgData "swarm"
  when createDirs (createDirectoryIfMissing True swarmData)
  pure swarmData

-- | Get path to swarm history, optionally creating necessary
--   directories. This could fail if user has bad permissions
--   on his own $HOME or $XDG_DATA_HOME which is unlikely.
getSwarmHistoryPath :: Bool -> IO FilePath
getSwarmHistoryPath createDirs =
  (</> "history") <$> getSwarmDataPath createDirs

-- | Read all the .txt files in the data/ directory.
readAppData :: IO (Map Text Text)
readAppData = do
  d <- getDataDir
  fs <-
    filter ((== ".txt") . takeExtension)
      <$> ( listDirectory d `catch` \e ->
              hPutStr stderr (show (e :: IOException)) >> return []
          )
  M.fromList . mapMaybe sequenceA
    <$> forM fs (\f -> (into @Text (dropExtension f),) <$> readFileMayT (d </> f))

------------------------------------------------------------
-- Some Text-y stuff

-- | Predicate to test for characters which can be part of a valid
--   identifier: alphanumeric, underscore, or single quote.
--
-- >>> isIdentChar 'A' && isIdentChar 'b' && isIdentChar '9'
-- True
-- >>> isIdentChar '_' && isIdentChar '\''
-- True
-- >>> isIdentChar '$' || isIdentChar '.' || isIdentChar ' '
-- False
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | @replaceLast r t@ replaces the last word of @t@ with @r@.
--
-- >>> :set -XOverloadedStrings
-- >>> replaceLast "foo" "bar baz quux"
-- "bar baz foo"
-- >>> replaceLast "move" "(make"
-- "(move"
replaceLast :: Text -> Text -> Text
replaceLast r t = T.append (T.dropWhileEnd isIdentChar t) r

------------------------------------------------------------
-- Some language-y stuff

-- | Reflow text by removing newlines and condensing whitespace.
reflow :: Text -> Text
reflow = T.unwords . T.words

-- | Prepend a noun with the proper indefinite article (\"a\" or \"an\").
indefinite :: Text -> Text
indefinite w = MM.indefiniteDet w <+> w

-- | Prepend a noun with the proper indefinite article, and surround
--   the noun in single quotes.
indefiniteQ :: Text -> Text
indefiniteQ w = MM.indefiniteDet w <+> squote w

-- | Combine the subject word with the simple present tense of the verb.
--
-- Only some irregular verbs are handled, but it should be enough
-- to scrap some error message boilerplate and have fun!
--
-- >>> :set -XOverloadedStrings
-- >>> singularSubjectVerb "I" "be"
-- "I am"
-- >>> singularSubjectVerb "he" "can"
-- "he can"
-- >>> singularSubjectVerb "The target robot" "do"
-- "The target robot does"
singularSubjectVerb :: Text -> Text -> Text
singularSubjectVerb sub verb
  | verb == "be" = case toUpper sub of
    "I" -> "I am"
    "YOU" -> sub <+> "are"
    _ -> sub <+> "is"
  | otherwise = sub <+> (if is3rdPerson then verb3rd else verb)
 where
  is3rdPerson = toUpper sub `notElem` ["I", "YOU"]
  verb3rd
    | verb == "have" = "has"
    | verb == "can" = "can"
    | otherwise = fst $ MM.defaultVerbStuff verb

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

deriving instance FromJSONKey (V2 Int64)
deriving instance ToJSONKey (V2 Int64)

deriving instance FromJSON TimeSpec
deriving instance ToJSON TimeSpec

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
l <<.= b = l %%= (,b)
{-# INLINE (<<.=) #-}

(<>=) :: (Has (State s) sig m, Semigroup a) => ASetter' s a -> a -> m ()
l <>= a = modify (l <>~ a)
{-# INLINE (<>=) #-}
