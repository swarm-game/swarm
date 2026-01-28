{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data types to represent Swarm-lang import locations.  This internal
-- module exports all constructors, including the constructor for
-- 'ImportDir', which is not exported from
-- "Swarm.Language.Syntax.Import" in order to guarantee internal
-- invariants.  Do not use this module directly unless you know what
-- you are doing and have a good reason.
module Swarm.Language.Syntax.Import.Internal where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Data (Data (..), Typeable)
import Data.Hashable (Hashable (..))
import Data.List (intercalate)
import Data.Text (Text)
import GHC.Generics
import Prettyprinter (hcat, pretty, punctuate, slash)
import Swarm.Pretty
import System.FilePath (joinPath, pathSeparator, (</>))
import Witch (into)

------------------------------------------------------------
-- Import phases

-- | Import locations can be raw, or fully resolved to a canonical URL
--   or absolute path.
type data ImportPhase where
  Raw :: ImportPhase
  Resolved :: ImportPhase

------------------------------------------------------------
-- Anchor

-- | A fully resolved anchor can be 'Root' (the filesystem root),
--   'Drive' (a drive letter on Windows), or 'Web', representing a
--   site root like @https://github.com/@.
data RAnchor where
  -- | The filesystem root.
  Root_ :: RAnchor
  -- | A drive letter, e.g. 'D:\\'
  Drive_ :: Char -> RAnchor
  -- | A web address.  The text represents scheme + authority, /e.g./
  -- "https://github.com"
  Web_ :: Text -> RAnchor
  deriving (Eq, Ord, Show, Generic, Data, FromJSON, ToJSON, Hashable)

-- | An unresolved anchor is an anchor which is a convenient shorthand
--   for a fully resolved path.  We can parse such anchors but will
--   then fully resolve/canonicalize them to a 'RAnchor'.
data UAnchor where
  -- | Relative to local dir.  The @Int@ is the number of levels to go
  --   up, *e.g.* 2 means "../..".  0 means the current dir, /i.e./
  --   "."
  Local_ :: Int -> UAnchor
  -- | Relative to the home directory, /i.e./ "~".
  Home_ :: UAnchor
  -- | Relative to the swarm data directory, /i.e./ "~swarm".
  Swarm_ :: UAnchor
  deriving (Eq, Ord, Show, Generic, Data, FromJSON, ToJSON, Hashable)

-- | An "anchor" is the root location from which to interpret a path:
--   an absolute path, a web URL, a local path (i.e. relative to CWD),
--   or the home directory.
--
--   In the future, if we wanted to have some kind of standard
--   library, we could add a fifth kind of anchor which refers to the
--   location of the stdlib files.
type family Anchor (phase :: ImportPhase) = result | result -> phase where
  Anchor Raw = Either UAnchor RAnchor
  Anchor Resolved = RAnchor

pattern Root :: Anchor Raw
pattern Root = Right Root_

pattern Drive :: Char -> Anchor Raw
pattern Drive c = Right (Drive_ c)

pattern Web :: Text -> Anchor Raw
pattern Web t = Right (Web_ t)

pattern Local :: Int -> Anchor Raw
pattern Local n = Left (Local_ n)

pattern Home :: Anchor Raw
pattern Home = Left Home_

pattern Swarm :: Anchor Raw
pattern Swarm = Left Swarm_

{-# COMPLETE Root, Drive, Web, Local, Home, Swarm #-}

instance PrettyPrec (Either UAnchor RAnchor) where
  prettyPrec p = either (prettyPrec p) (prettyPrec p)

instance PrettyPrec RAnchor where
  prettyPrec _ = \case
    Root_ -> mempty
    Drive_ c -> pretty [c, ':']
    Web_ w -> pretty w

instance PrettyPrec UAnchor where
  prettyPrec _ = \case
    Local_ n -> hcat $ punctuate slash (replicate n "..")
    Home_ -> "~"
    Swarm_ -> "~swarm"

-- | A concrete representation of a path: either a local path or a URL.
data Path = LocalPath FilePath | URL String
  deriving (Eq, Ord, Show)

pathToString :: Path -> String
pathToString = \case
  LocalPath f -> f
  URL u -> u

-- | Turn an 'Anchor' into a concrete path.
anchorToPath :: Anchor Resolved -> String
anchorToPath = \case
  Root_ -> [pathSeparator]
  Drive_ c -> [c, ':', pathSeparator]
  Web_ w -> into @String w

------------------------------------------------------------
-- ImportDir

-- | An import directory consists of an 'Anchor' together with a path.
--
--   The decision to represent directories as a list of
--   components, rather than using e.g. @FilePath@ or @OsPath@, is
--   deliberate: it allows us to decouple the syntax of import
--   specifications in Swarm-lang from the actual OS-specific path
--   syntax.  For example, we can allow using either front- or
--   backslashes as path separators in import expressions regardless
--   of the OS, then turn it into an actual OS path in an appropriate
--   way.
--
--   The constructor of 'ImportDir' is not exported.  This is a
--   deliberate choice to enforce safety: we enforce as an invariant
--   that 'ImportDir' values are always normalized.  To create one,
--   use the 'mkImportDir' smart constructor. To extract information,
--   use 'withImportDir'.
data ImportDir (phase :: ImportPhase) = ImportDir (Anchor phase) [Text]

deriving instance Eq (Anchor phase) => Eq (ImportDir phase)
deriving instance Ord (Anchor phase) => Ord (ImportDir phase)
deriving instance Show (Anchor phase) => Show (ImportDir phase)
deriving instance Generic (Anchor phase) => Generic (ImportDir phase)
deriving instance (Typeable phase, Data (Anchor phase)) => Data (ImportDir phase)
deriving instance (Generic (Anchor phase), FromJSON (Anchor phase)) => FromJSON (ImportDir phase)
deriving instance (Generic (Anchor phase), ToJSON (Anchor phase)) => ToJSON (ImportDir phase)
deriving instance (Generic (Anchor phase), Hashable (Anchor phase)) => Hashable (ImportDir phase)

instance PrettyPrec (Anchor phase) => PrettyPrec (ImportDir phase) where
  prettyPrec _ (ImportDir anchor ps) = hcat $ punctuate slash (ppr anchor : map pretty ps)

-- | Convenient shortcut for the 'ImportDir' representing the user's
--   home directory.
homeDir :: ImportDir Raw
homeDir = ImportDir Home []

-- | Convenient shortcut for the 'ImportDir' representing the current
--   working directory.
currentDir :: ImportDir Raw
currentDir = ImportDir (Local 0) []

-- | Smart constructor for 'ImportDir' which ensures that it is
-- normalized.
mkImportDir :: Anchor Raw -> [Text] -> ImportDir Raw
mkImportDir a p = canonicalizeImportDir $ ImportDir a p

-- | Destructor/eliminator for 'ImportDir'.  Since the constructor for
--   'ImportDir' is not exported, this is the primary way to access
--   the contents.
withImportDir :: (Anchor phase -> [Text] -> r) -> ImportDir phase -> r
withImportDir f (ImportDir a p) = f a p

-- | The Semigroup instance for 'ImportDir' interprets the second in
--   the context of the first.  If the second 'ImportDir' is not local
--   (/i.e./ if its anchor is 'Web', 'Home', 'Swarm', or 'Absolute'), the first
--   is simply ignored and the second is returned unchanged.  If the
--   second 'ImportDir' is 'Local', computes a location which uses the
--   first location as an initial anchor and then interprets the
--   second relative to that.
--
--   See
--   https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md#chaining-directories
--   for inspiration.
instance Semigroup (ImportDir Raw) where
  _ <> d@(ImportDir Root _) = d
  _ <> d@(ImportDir (Drive {}) _) = d
  _ <> d@(ImportDir (Web {}) _) = d
  _ <> d@(ImportDir Home _) = d
  _ <> d@(ImportDir Swarm _) = d
  ImportDir a p1 <> ImportDir (Local n) p2 = mkImportDir a (p1 ++ replicate n ".." ++ p2)

-- | The identity 'ImportDir' is one with a local anchor and empty
--   path component list.
instance Monoid (ImportDir Raw) where
  mempty = ImportDir (Local 0) []

-- | Turn an 'ImportDir' into a concrete path.
dirToPath :: ImportDir Resolved -> Path
dirToPath = withImportDir $ \a p ->
  case a of
    -- URLs must specifically use front slashes as separators
    Web_ {} -> URL $ intercalate "/" (anchorToPath a : map (into @FilePath) p)
    -- Otherwise, use whatever is appropriate for the OS
    _ -> LocalPath $ anchorToPath a </> joinPath (map (into @FilePath) p)

------------------------------------------------------------
-- Canonicalization

-- | Canonicalize an import directory.  A canonical import directory
--    never contains ".." (except at the beginning in the form of a
--    'Local' 'Anchor') or ".".
--
--   This is not meant to be exported, but is used as part of the
--   smart constructor 'mkImportDir'.
--
-- >>> :set -XOverloadedStrings
-- >>> mkImportDir Home ["."]
-- ImportDir (Left Home_) []
-- >>> mkImportDir Home ["a","b"]
-- ImportDir (Left Home_) ["a","b"]
-- >>> mkImportDir Home ["a","..","b"]
-- ImportDir (Left Home_) ["b"]
-- >>> mkImportDir Home ["a","..","..","b"]
-- ImportDir (Left Home_) ["b"]
-- >>> mkImportDir (Local 0) ["a", "..", "b"]
-- ImportDir (Left (Local_ 0)) ["b"]
-- >>> mkImportDir (Local 0) ["a", "..", "..", "b"]
-- ImportDir (Left (Local_ 1)) ["b"]
-- >>> mkImportDir (Local 0) ["a", "..", "..", "b", ".", "c"]
-- ImportDir (Left (Local_ 1)) ["b","c"]
-- >>> mkImportDir (Local 1) ["a", "..", "..", "b", "c", "..", "d", "..", "..", ".."]
-- ImportDir (Left (Local_ 3)) []
canonicalizeImportDir :: ImportDir Raw -> ImportDir Raw
canonicalizeImportDir (ImportDir a p) = case (a, canonicalizeDir (0, [], p)) of
  (Local n, (m, p')) -> ImportDir (Local (n + m)) p'
  (_, (_, p')) -> ImportDir a p'
 where
  canonicalizeDir :: (Int, [Text], [Text]) -> (Int, [Text])
  canonicalizeDir = \case
    (n, pre, "." : ps) -> canonicalizeDir (n, pre, ps)
    (n, [], ".." : ps) -> canonicalizeDir (n + 1, [], ps)
    (n, _ : xs, ".." : ps) -> canonicalizeDir (n, xs, ps)
    (n, pre, d : ps) -> canonicalizeDir (n, d : pre, ps)
    (n, pre, []) -> (n, reverse pre)

------------------------------------------------------------
-- ImportLoc

-- | A location from which to import a file containing Swarm code,
--   consisting of a directory paired with a filename.
--
--   Parameterized by phase so we can be sure to canonicalize +
--   process imports in a typesafe way.
data ImportLoc (phase :: ImportPhase) = ImportLoc
  { importDir :: ImportDir phase
  , importFile :: Text
  }

deriving instance Eq (Anchor phase) => Eq (ImportLoc phase)
deriving instance Ord (Anchor phase) => Ord (ImportLoc phase)
deriving instance Show (Anchor phase) => Show (ImportLoc phase)
deriving instance Generic (Anchor phase) => Generic (ImportLoc phase)
deriving instance (Typeable phase, Data (Anchor phase)) => Data (ImportLoc phase)
deriving instance (Generic (Anchor phase), FromJSON (Anchor phase)) => FromJSON (ImportLoc phase)
deriving instance (Generic (Anchor phase), ToJSON (Anchor phase)) => ToJSON (ImportLoc phase)
deriving instance (Generic (Anchor phase), ToJSON (Anchor phase)) => ToJSONKey (ImportLoc phase)
deriving instance (Generic (Anchor phase), Hashable (Anchor phase)) => Hashable (ImportLoc phase)

instance PrettyPrec (Anchor phase) => PrettyPrec (ImportLoc phase) where
  prettyPrec _ (ImportLoc d f) = ppr d <> "/" <> pretty f

-- | Get the 'Anchor' for an 'ImportLoc'.
importAnchor :: ImportLoc phase -> Anchor phase
importAnchor = withImportDir const . importDir

-- | Turn an 'ImportLoc' into a concrete path.
locToPath :: ImportLoc Resolved -> Path
locToPath (ImportLoc d f) =
  case dirToPath d of
    LocalPath dp -> LocalPath (dp </> into @FilePath f)
    URL url -> URL (url ++ "/" ++ into @String f)

-- | Turn an 'ImportLoc' into a concrete FilePath (or URL).
locToFilePath :: ImportLoc Resolved -> FilePath
locToFilePath = pathToString . locToPath

-- | Append an import location to the end of an import dir, resulting
--   in a new import location.  That is, interpret the import location
--   in the context of the given dir.
(<//>) :: ImportDir Raw -> ImportLoc Raw -> ImportLoc Raw
d1 <//> ImportLoc d2 f = ImportLoc (d1 <> d2) f
