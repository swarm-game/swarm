{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data types to represent Swarm-lang import locations.
module Swarm.Language.Syntax.Import (
  -- * Import phase
  ImportPhase (..),

  -- * Anchors
  Anchor (..),

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
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Data (Data (..), Typeable)
import Data.Data qualified as Data
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (hcat, pretty, punctuate, slash)
import Swarm.Pretty
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (joinPath, splitDirectories, (</>))
import Unsafe.Coerce (unsafeCoerce)
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

-- | An "anchor" from which to interpret a path.
data Anchor (phase :: ImportPhase) where
  -- | Absolute, /i.e./ relative to the filesystem root.
  Absolute :: Anchor phase
  -- | Text represents scheme + authority, /e.g./ "https://github.com"
  Web :: Text -> Anchor phase
  -- | Relative to local dir.  The @Int@ is the number of levels to go
  --   up, *e.g.* 2 means "../..".  0 means the current dir, /i.e./
  --   "."
  --
  --   Local anchors can only be raw; they get resolved into an
  --   Absolute path.
  Local :: Int -> Anchor Raw
  -- | Relative to the home directory, /i.e./ "~".  Home anchor can
  --   only be raw; it will get resolved into an Absolute path.
  Home :: Anchor Raw

deriving instance Eq (Anchor phase)
deriving instance Ord (Anchor phase)
deriving instance Show (Anchor phase)

instance Typeable phase => Data (Anchor phase) where
  gfoldl _ z Absolute = z Absolute
  gfoldl k z (Web t) = k (z Web) t
  gfoldl k z (Local n) = k (z Local) n
  gfoldl _ z Home = z Home

  toConstr Absolute = constrAbsolute
  toConstr (Web _) = constrWeb
  toConstr (Local _) = constrLocal
  toConstr Home = constrHome
  dataTypeOf _ = dataTypeAnchor

  -- See Note [Data Anchor instance]
  gunfold k z c = error "Can't implement gunfold for Anchor"

constrAbsolute :: Data.Constr
constrAbsolute = Data.mkConstrTag dataTypeAnchor "Absolute" 1 [] Data.Prefix

constrWeb :: Data.Constr
constrWeb = Data.mkConstrTag dataTypeAnchor "Web" 2 [] Data.Prefix

constrLocal :: Data.Constr
constrLocal = Data.mkConstrTag dataTypeAnchor "Local" 3 [] Data.Prefix

constrHome :: Data.Constr
constrHome = Data.mkConstrTag dataTypeAnchor "Home" 4 [] Data.Prefix

dataTypeAnchor :: Data.DataType
dataTypeAnchor = Data.mkDataType "Swarm.Language.Syntax.Import.Anchor" [constrAbsolute, constrWeb, constrLocal, constrHome]

-- ~~~~ Note [Data Anchor instance]
--
-- Anchor is a GADT to ensure that after import resolution, all import
-- locations are resolved/canonicalized to have either Absolute or Web
-- anchors.  The Local and Home constructors specifically construct an
-- Anchor Raw, so they cannot be used to construct an Anchor Resolved.
--
-- However, making it a GADT in this way means we cannot auto-derive a
-- Data Anchor instance.  Implementing one by hand, as above, reveals
-- that the real problem is gunfold, which requires us to construct
-- Anchor values using an arbitrary constructor. However, we can't
-- really do that, since which constructors are valid depend on the
-- type being constructed.
--
-- We could make two instances, one for Anchor Raw and one for Anchor
-- Resolved, which would allow us to avoid the unsafeCoerce.  But then
-- we would be committed to making separate instances for every type.
--
-- However, the Data instance is ultimately needed for things such as:
--
--   - Functions like `asTree` and `measureAstSize`, which take an
--     existing syntax tree and analyze it generically
--   - Populating syntax trees with comments, which generically
--     traverses through a syntax tree and inserts comments in
--     appropriate nodes
--   - Syntax quasiquoters, which parse a string into a syntax tree
--     and then generically turn the syntax tree into Haskell syntax,
--     via `dataToExpQ`.
--
-- All of these applications *consume* syntax trees generically, which
-- only needs `gfoldl` (which is unproblematic) rather than `gunfold`.
-- We could implement `gunfold` using `unsafeCoerce` in a way that
-- would probably work fine---but it seems safer to simply leave
-- `gunfold` unimplemented, so that Swarm will fail loudly if we ever
-- violate the assumption that we do not need it.

instance FromJSON (Anchor phase) where
  parseJSON = undefined -- XXX can't auto-derive due to GADT; implement by hand?
instance ToJSON (Anchor phase) where
  toJSON = undefined -- XXX
instance Hashable (Anchor phase) where
  hashWithSalt = undefined -- XXX

instance PrettyPrec (Anchor phase) where
  prettyPrec _ = \case
    Absolute -> mempty
    Web w -> pretty w
    Local n -> hcat $ punctuate slash (replicate n "..")
    Home -> "~"

-- | Turn an 'Anchor' into a concrete 'FilePath' (or URL).
anchorToFilePath :: Anchor Resolved -> FilePath
anchorToFilePath = \case
  Absolute -> "/"
  Web w -> into @FilePath w

-- XXX
unresolveAnchor :: Anchor phase -> Anchor Raw
unresolveAnchor = \case
  Absolute -> Absolute
  Web t -> Web t
  Local n -> Local n
  Home -> Home

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
  deriving (Eq, Ord, Show, Generic, Data, FromJSON, ToJSON, Hashable)

instance PrettyPrec (ImportDir phase) where
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
mkImportDir :: Anchor phase -> [Text] -> ImportDir phase
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
instance Semigroup (ImportDir phase) where
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

-- XXX
unresolveImportDir :: ImportDir phase -> ImportDir Raw
unresolveImportDir (ImportDir a p) = ImportDir (unresolveAnchor a) p

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
canonicalizeImportDir :: ImportDir phase -> ImportDir phase
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
  deriving (Generic, Data, Eq, Ord, Show, Hashable, ToJSON)

instance PrettyPrec (ImportLoc phase) where
  prettyPrec _ (ImportLoc d f) = ppr d <> "/" <> pretty f

-- | Get the 'Anchor' for an 'ImportLoc'.
importAnchor :: ImportLoc phase -> Anchor phase
importAnchor = withImportDir const . importDir

-- | Turn an 'ImportLoc' into a concrete 'FilePath' (or URL).
locToFilePath :: ImportLoc Resolved -> FilePath
locToFilePath (ImportLoc d f) = dirToFilePath d </> into @FilePath f

-- XXX
(<//>) :: ImportDir phase -> ImportLoc phase -> ImportLoc phase
d1 <//> ImportLoc d2 f = ImportLoc (d1 <> d2) f

------------------------------------------------------------
-- Import resolution

-- XXX simply assume web resources exist without checking?  + require them to be fully named...?

-- | Resolve an import directory, turning it into an absolute path.
resolveImportDir :: Has (Lift IO) sig m => ImportDir Raw -> m (ImportDir Resolved)
resolveImportDir (ImportDir a p) = case a of
  Web t -> pure $ ImportDir (Web t) p
  Local n -> do
    cwd <- drop 1 . reverse . drop n . reverse . splitDirectories <$> sendIO getCurrentDirectory
    pure $ ImportDir Absolute (map (into @Text) cwd ++ p)
  Home -> do
    home <- drop 1 . splitDirectories <$> sendIO getHomeDirectory
    pure $ ImportDir Absolute (map (into @Text) home ++ p)
  Absolute -> pure $ ImportDir Absolute p

-- | Check whether a given 'ImportLoc' in fact exists.  Note that, for
--   the sake of efficiency, this simply assumes that any 'Web'
--   resource exists without checking; all other locations will
--   actually be checked.
doesLocationExist :: (Has (Lift IO) sig m) => ImportLoc Resolved -> m Bool
doesLocationExist loc = do
  let fp = locToFilePath loc
  case importAnchor loc of
    Web {} -> pure True
    _ -> sendIO $ doesFileExist fp

-- XXX need to be able to resolve "local" to something in a standard
-- Swarm data location, instead of just looking up CWD??

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

-- XXX
unresolveImportLoc :: ImportLoc a -> ImportLoc Raw
unresolveImportLoc (ImportLoc d f) = ImportLoc (unresolveImportDir d) f
  
