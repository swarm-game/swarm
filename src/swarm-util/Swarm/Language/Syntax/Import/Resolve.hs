{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Resolving Swarm-lang imports to canonical locations.
module Swarm.Language.Syntax.Import.Resolve (
  Unresolvable,
  unresolveImportDir,
  unresolveImportLoc,
  resolveImportDir,
  resolveImportLoc,
)
where

import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw)
import Data.Text (Text)
import Swarm.Failure (AssetData (Script), SystemFailure)
import Swarm.Language.Syntax.Import.Internal
import Swarm.ResourceLoading (getDataDirThrow)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (splitDirectories)
import Witch (into)

-- | Turn any anchor into a raw anchor.
class Unresolvable (phase :: ImportPhase) where
  unresolve :: Anchor phase -> Anchor Raw

instance Unresolvable Raw where
  unresolve = id

instance Unresolvable Resolved where
  unresolve = Right

-- | Turn any import dir back into a raw one.
unresolveImportDir :: Unresolvable phase => ImportDir phase -> ImportDir Raw
unresolveImportDir (ImportDir a p) = ImportDir (unresolve a) p

-- | Turn a resolved import loc back into a raw one.
unresolveImportLoc :: Unresolvable phase => ImportLoc phase -> ImportLoc Raw
unresolveImportLoc (ImportLoc d f) = ImportLoc (unresolveImportDir d) f

-- | Resolve an import directory, turning it into an absolute path.
resolveImportDir :: (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) => ImportDir Raw -> m (ImportDir Resolved)
resolveImportDir (ImportDir a p) = case a of
  Local n -> mkAbsolute . reverse . drop n . reverse . splitDirectories <$> sendIO getCurrentDirectory
  Home -> mkAbsolute . splitDirectories <$> sendIO getHomeDirectory
  Swarm -> mkAbsolute . splitDirectories <$> getDataDirThrow Script ""
  Web t -> pure $ ImportDir (Web_ t) p
  Root -> pure $ ImportDir Root_ p
  Drive c -> pure $ ImportDir (Drive_ c) p
 where
  readAnchor (d : ':' : _) = Drive_ d
  readAnchor _ = Root_

  mkAbsolute = \case
    [] -> ImportDir Root_ p
    (root : dirs) -> ImportDir (readAnchor root) (map (into @Text) dirs ++ p)

-- | Check whether a given 'ImportLoc' in fact exists.  Note that, for
--   the sake of efficiency, this simply assumes that any 'Web'
--   resource exists without checking; all other locations will
--   actually be checked.  This means, for example, that web imports
--   must have an `.sw` extension fully written out.
doesLocationExist :: (Has (Lift IO) sig m) => ImportLoc Resolved -> m Bool
doesLocationExist loc = case locToPath loc of
  URL _ -> pure True
  LocalPath fp -> sendIO $ doesFileExist fp

-- | Resolve an import location, by turning the path into an absolute
--   path, and optionally adding a .sw suffix to the file name.
resolveImportLoc :: (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) => ImportLoc Raw -> m (ImportLoc Resolved)
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
