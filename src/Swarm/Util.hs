{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A random collection of small, useful functions that are (or could
-- be) used throughout the code base.
module Swarm.Util (
  -- * Miscellaneous utilities
  (?),
  sortPair,
  maxOn,
  maximum0,
  cycleEnum,
  listEnums,
  indexWrapNonEmpty,
  uniq,
  binTuples,
  histogram,
  findDup,
  both,
  allEqual,

  -- * Directory utilities
  readFileMay,
  readFileMayT,

  -- * Text utilities
  isIdentChar,
  replaceLast,
  failT,
  showT,

  -- * English language utilities
  reflow,
  quote,
  squote,
  bquote,
  parens,
  brackets,
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
  guardRight,
  simpleErrorHandle,

  -- * Template Haskell utilities
  liftText,

  -- * Lens utilities
  (%%=),
  (<%=),
  (<+=),
  (<<.=),
  (<>=),
  _NonEmpty,

  -- * Set utilities
  removeSupersets,
  smallHittingSet,
) where

import Control.Algebra (Has)
import Control.Effect.State (State, modify, state)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens (ASetter', Lens', LensLike, LensLike', Over, lens, (<>~))
import Control.Monad (unless, (<=<))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Char (isAlphaNum)
import Data.Either.Validation
import Data.List (foldl', maximumBy, partition)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text, toUpper)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple (swap)
import Data.Yaml
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import NLP.Minimorph.English qualified as MM
import NLP.Minimorph.Util ((<+>))
import System.Clock (TimeSpec)
import System.IO.Error (catchIOError)
import Witch (from)

infixr 1 ?
infix 4 %%=, <+=, <%=, <<.=, <>=

-- | A convenient infix flipped version of 'fromMaybe': @Just a ? b =
--   a@, and @Nothing ? b = b@. It can also be chained, as in @x ? y ?
--   z ? def@, which takes the value inside the first @Just@,
--   defaulting to @def@ as a last resort.
(?) :: Maybe a -> a -> a
(?) = flip fromMaybe

-- | Ensure the smaller value in a pair is the first element.
sortPair :: Ord b => (b, b) -> (b, b)
sortPair (x, y) = if x <= y then (x, y) else (y, x)

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

listEnums :: (Enum e, Bounded e) => [e]
listEnums = [minBound .. maxBound]

-- | Guaranteed to yield an element of the list
indexWrapNonEmpty :: Integral b => NonEmpty a -> b -> a
indexWrapNonEmpty list idx =
  NE.toList list !! fromIntegral wrappedIdx
 where
  wrappedIdx = idx `mod` fromIntegral (NE.length list)

-- | Drop repeated elements that are adjacent to each other.
--
-- >>> uniq []
-- []
-- >>> uniq [1..5]
-- [1,2,3,4,5]
-- >>> uniq (replicate 10 'a')
-- "a"
-- >>> uniq "abbbccd"
-- "abcd"
uniq :: Eq a => [a] -> [a]
uniq = \case
  [] -> []
  (x : xs) -> x : uniq (dropWhile (== x) xs)

-- | Place the second element of the tuples into bins by
-- the value of the first element.
binTuples ::
  (Foldable t, Ord a) =>
  t (a, b) ->
  Map a (NE.NonEmpty b)
binTuples = foldr f mempty
 where
  f = uncurry (M.insertWith (<>)) . fmap pure

-- | Count occurrences of a value
histogram ::
  (Foldable t, Ord a) =>
  t a ->
  Map a Int
histogram = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty

-- | Find a duplicate element within the list, if any exists.
findDup :: Ord a => [a] -> Maybe a
findDup = go S.empty
 where
  go _ [] = Nothing
  go seen (a : as)
    | a `S.member` seen = Just a
    | otherwise = go (S.insert a seen) as

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

allEqual :: (Ord a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

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

-- | Fail with a Text-based message, made out of phrases to be joined
--   by spaces.
failT :: MonadFail m => [Text] -> m a
failT = fail . from @Text . T.unwords

-- | Show a value, but as Text.
showT :: Show a => a -> Text
showT = from @String . show

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

-- | Surround some text in backticks.
bquote :: Text -> Text
bquote t = T.concat ["`", t, "`"]

-- | Surround some text in parentheses.
parens :: Text -> Text
parens t = T.concat ["(", t, ")"]

-- | Surround some text in square brackets.
brackets :: Text -> Text
brackets t = T.concat ["[", t, "]"]

-- | Make a list of things with commas and the word "and".
commaList :: [Text] -> Text
commaList [] = ""
commaList [t] = t
commaList [s, t] = T.unwords [s, "and", t]
commaList ts = T.unwords $ map (`T.append` ",") (init ts) ++ ["and", last ts]

------------------------------------------------------------
-- Some orphan instances

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

guardRight :: Text -> Either Text a -> ExceptT Text IO a
guardRight what i = i `isRightOr` (\e -> "Failed to " <> what <> ": " <> e)

simpleErrorHandle :: ExceptT Text IO a -> IO a
simpleErrorHandle = either (fail . T.unpack) pure <=< runExceptT

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

------------------------------------------------------------
-- Other lens utilities

_NonEmpty :: Lens' (NonEmpty a) (a, [a])
_NonEmpty = lens (\(x :| xs) -> (x, xs)) (const (uncurry (:|)))

------------------------------------------------------------
-- Some set utilities

-- | Remove any sets which are supersets of other sets.  In other words,
--   (1) no two sets in the output are in a subset relationship
--   (2) every element in the input is a superset of some element in the output.
--
-- >>> import qualified Data.Set as S
-- >>> rss = map S.toList . S.toList . removeSupersets . S.fromList . map S.fromList
--
-- >>> rss [[1,2,3], [1]]
-- [[1]]
--
-- >>> rss [[1,2,3], [2,4], [2,3]]
-- [[2,3],[2,4]]
--
-- >>> rss [[], [1], [2,3]]
-- [[]]
--
-- >>> rss [[1,2], [1,3], [2,3]]
-- [[1,2],[1,3],[2,3]]
removeSupersets :: Ord a => Set (Set a) -> Set (Set a)
removeSupersets ss = S.filter (not . isSuperset) ss
 where
  isSuperset s = any (`S.isSubsetOf` s) (S.delete s ss)

-- | Given a list of /nonempty/ sets, find a hitting set, that is, a
--   set which has at least one element in common with each set in the
--   list.  It is not guaranteed to be the /smallest possible/ such
--   set, because that is NP-hard.  Instead, we use a greedy algorithm
--   that will give us a reasonably small hitting set: first, choose
--   all elements in singleton sets, since those must necessarily be
--   chosen.  Now take any sets which are still not hit, and find an
--   element which occurs in the largest possible number of remaining
--   sets. Add this element to the set of chosen elements, and filter
--   out all the sets it hits.  Repeat, choosing a new element to hit
--   the largest number of unhit sets at each step, until all sets are
--   hit.  This algorithm produces a hitting set which might be larger
--   than optimal by a factor of lg(m), where m is the number of sets
--   in the input.
--
-- >>> import qualified Data.Set as S
-- >>> shs = smallHittingSet . map S.fromList
--
-- >>> shs ["a"]
-- fromList "a"
--
-- >>> shs ["ab", "b"]
-- fromList "b"
--
-- >>> shs ["ab", "bc"]
-- fromList "b"
--
-- >>> shs ["acd", "c", "aef", "a"]
-- fromList "ac"
--
-- >>> shs ["abc", "abd", "acd", "bcd"]
-- fromList "cd"
--
-- Here is an example of an input for which @smallHittingSet@ does
-- /not/ produce a minimal hitting set. "bc" is also a hitting set and
-- is smaller.  b, c, and d all occur in exactly two sets, but d is
-- unluckily chosen first, leaving "be" and "ac" unhit and
-- necessitating choosing one more element from each.
--
-- >>> shs ["bd", "be", "ac", "cd"]
-- fromList "cde"
smallHittingSet :: Ord a => [Set a] -> Set a
smallHittingSet ss = go fixed (filter (S.null . S.intersection fixed) choices)
 where
  (fixed, choices) = first S.unions . partition ((== 1) . S.size) . filter (not . S.null) $ ss

  go !soFar [] = soFar
  go !soFar cs = go (S.insert best soFar) (filter (not . (best `S.member`)) cs)
   where
    best = mostCommon cs

  -- Given a nonempty collection of sets, find an element which is shared among
  -- as many of them as possible.
  mostCommon :: Ord a => [Set a] -> a
  mostCommon = fst . maximumBy (comparing snd) . M.assocs . M.fromListWith (+) . map (,1 :: Int) . concatMap S.toList
