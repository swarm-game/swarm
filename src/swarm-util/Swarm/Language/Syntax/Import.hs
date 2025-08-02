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
-- Data types to represent Swarm-lang import locations.
module Swarm.Language.Syntax.Import (
  -- * Import phase
  ImportPhase (..),

  -- * Anchors
  Anchor,
  RAnchor (..),
  UAnchor (..),
  Unresolvable,
  pattern Absolute,
  pattern Web,
  pattern Local,
  pattern Home,

  -- * ImportDir
  ImportDir,
  mkImportDir,
  withImportDir,

  -- ** Pre-defined dirs
  homeDir,
  currentDir,

  -- * ImportLoc
  ImportLoc (..),
  importAnchor,

  -- ** Utilities
  anchorToFilePath,
  dirToFilePath,
  locToFilePath,
  (<//>),
  unresolveImportDir,
  unresolveImportLoc,

  -- ** Resolution

  -- unsafeResolveImportLoc,
  resolveImportDir,
  resolveImportLoc,
) where

import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Data (Data (..), Typeable)
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import GHC.Generics
import Prettyprinter (hcat, pretty, punctuate, slash)
import Swarm.Pretty
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (joinPath, splitDirectories, (</>))
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

-- | A fully resolved anchor can only be one of two things: 'Absolute'
--   (the filesystem root), or 'Web', representing a site root like
--   @https://github.com/@.
data RAnchor where
  -- | Absolute, /i.e./ relative to the filesystem root.
  Absolute_ :: RAnchor
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

pattern Absolute :: Anchor Raw
pattern Absolute = Right Absolute_

pattern Web :: Text -> Anchor Raw
pattern Web t = Right (Web_ t)

pattern Local :: Int -> Anchor Raw
pattern Local n = Left (Local_ n)

pattern Home :: Anchor Raw
pattern Home = Left Home_

{-# COMPLETE Absolute, Web, Local, Home #-}

instance PrettyPrec (Either UAnchor RAnchor) where
  prettyPrec p = either (prettyPrec p) (prettyPrec p)

instance PrettyPrec RAnchor where
  prettyPrec _ = \case
    Absolute_ -> mempty
    Web_ w -> pretty w

instance PrettyPrec UAnchor where
  prettyPrec _ = \case
    Local_ n -> hcat $ punctuate slash (replicate n "..")
    Home_ -> "~"

-- | Turn an 'Anchor' into a concrete 'FilePath' (or URL).
anchorToFilePath :: Anchor Resolved -> FilePath
anchorToFilePath = \case
  Absolute_ -> "/"
  Web_ w -> into @FilePath w

-- | Turn any anchor into a raw anchor.
class Unresolvable (phase :: ImportPhase) where
  unresolve :: Anchor phase -> Anchor Raw

instance Unresolvable Raw where
  unresolve = id

instance Unresolvable Resolved where
  unresolve = Right

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
--   (/i.e./ if its anchor is 'Web', 'Home', or 'Absolute'), the first
--   is simply ignored and the second is returned unchanged.  If the
--   second 'ImportDir' is 'Local', computes a location which uses the
--   first location as an initial anchor and then interprets the
--   second relative to that.
--
--   See
--   https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md#chaining-directories
--   for inspiration.
instance Semigroup (ImportDir Raw) where
  _ <> d@(ImportDir Absolute _) = d
  _ <> d@(ImportDir (Web {}) _) = d
  _ <> d@(ImportDir Home _) = d
  ImportDir a p1 <> ImportDir (Local n) p2 = mkImportDir a (p1 ++ replicate n ".." ++ p2)

-- | The identity 'ImportDir' is one with a local anchor and empty
--   path component list.
instance Monoid (ImportDir Raw) where
  mempty = ImportDir (Local 0) []

-- | Turn an 'ImportDir' into a concrete 'FilePath' (or URL).
dirToFilePath :: ImportDir Resolved -> FilePath
dirToFilePath = withImportDir $ \a p ->
  anchorToFilePath a </> joinPath (map (into @FilePath) p)

-- | Turn any import dir back into a raw one.
unresolveImportDir :: Unresolvable phase => ImportDir phase -> ImportDir Raw
unresolveImportDir (ImportDir a p) = ImportDir (unresolve a) p

------------------------------------------------------------
-- Canonicalization

-- | Canonicalize an import directory.  A canonical import directory
--    never contains ".." (except at the beginning in the form of a
--    'Local' 'Anchor') or ".".
--
--   This is not meant to be exported, but is used as part of the
--   smart constructor 'mkImportDir'.
--
--  >>> canonicalizeImportDir (ImportDir Home ["."])
--  ImportDir {importAnchor = Home, importPath = []}
--  >>> canonicalizeImportDir (ImportDir Home ["a","b"])
--  ImportDir {importAnchor = Home, importPath = ["a","b"]}
--  >>> canonicalizeImportDir (ImportDir Home ["a","..","b"])
--  ImportDir {importAnchor = Home, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir Home ["a","..","..","b"])
--  ImportDir {importAnchor = Home, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "b"])
--  ImportDir {importAnchor = Local 0, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "..", "b"])
--  ImportDir {importAnchor = Local 1, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "..", "b", ".", "c"])
--  ImportDir {importAnchor = Local 1, importPath = ["b","c"]}
--  >>> canonicalizeImportDir (ImportDir (Local 1) ["a", "..", "..", "b", "c", "..", "d", "..", "..", ".."])
--  ImportDir {importAnchor = Local 3, importPath = []}
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

-- | Turn an 'ImportLoc' into a concrete 'FilePath' (or URL).
locToFilePath :: ImportLoc Resolved -> FilePath
locToFilePath (ImportLoc d f) = dirToFilePath d </> into @FilePath f

-- | Append an import location to the end of an import dir, resulting
--   in a new import location.  That is, interpret the import location
--   in the context of the given dir.
(<//>) :: ImportDir Raw -> ImportLoc Raw -> ImportLoc Raw
d1 <//> ImportLoc d2 f = ImportLoc (d1 <> d2) f

------------------------------------------------------------
-- Import resolution

-- | Resolve an import directory, turning it into an absolute path.
resolveImportDir :: Has (Lift IO) sig m => ImportDir Raw -> m (ImportDir Resolved)
resolveImportDir (ImportDir a p) = case a of
  Web t -> pure $ ImportDir (Web_ t) p
  Local n -> do
    cwd <- drop 1 . reverse . drop n . reverse . splitDirectories <$> sendIO getCurrentDirectory
    pure $ ImportDir Absolute_ (map (into @Text) cwd ++ p)
  Home -> do
    home <- drop 1 . splitDirectories <$> sendIO getHomeDirectory
    pure $ ImportDir Absolute_ (map (into @Text) home ++ p)
  Absolute -> pure $ ImportDir Absolute_ p

-- | Check whether a given 'ImportLoc' in fact exists.  Note that, for
--   the sake of efficiency, this simply assumes that any 'Web'
--   resource exists without checking; all other locations will
--   actually be checked.  This means, for example, that web imports
--   must have an `.sw` extension fully written out.
doesLocationExist :: (Has (Lift IO) sig m) => ImportLoc Resolved -> m Bool
doesLocationExist loc = do
  let fp = locToFilePath loc
  case importAnchor loc of
    Web_ {} -> pure True
    _ -> sendIO $ doesFileExist fp

-- | Resolve an import location, by turning the path into an absolute
--   path, and optionally adding a .sw suffix to the file name.
resolveImportLoc :: Has (Lift IO) sig m => ImportLoc Raw -> m (ImportLoc Resolved)
resolveImportLoc (ImportLoc d f) = do
  d' <- resolveImportDir d
  let loc' = ImportLoc d' f
      loc'sw = ImportLoc d' (f <> ".sw")
  e1 <- doesLocationExist loc'
  e2 <- doesLocationExist loc'sw
  case (e1, e2) of
    -- Only automatically add .sw extension if the original location
    -- does not exist, but the location with .sw appended does
    (False, True) -> pure loc'sw
    _ -> pure loc'

-- | Turn a resolved import loc back into a raw one.
unresolveImportLoc :: Unresolvable phase => ImportLoc phase -> ImportLoc Raw
unresolveImportLoc (ImportLoc d f) = ImportLoc (unresolveImportDir d) f
