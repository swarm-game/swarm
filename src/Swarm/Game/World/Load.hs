{-# LANGUAGE DataKinds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading world descriptions from `worlds/*.world`.
module Swarm.Game.World.Load where

import Control.Algebra (Has)
import Control.Arrow (left)
import Control.Carrier.Accum.FixedStrict (Accum)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Reader (runReader)
import Control.Effect.Throw (Throw, liftEither)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Text (Text)
import Swarm.Game.Entity (EntityMap)
import Swarm.Game.Failure (Asset (..), AssetData (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Game.ResourceLoading (getDataDirSafe)
import Swarm.Game.World.Parse (parseWExp, runParser)
import Swarm.Game.World.Typecheck
import Swarm.Language.Pretty (prettyText)
import Swarm.Util (acquireAllWithExt)
import Swarm.Util.Effect (throwToWarning, withThrow)
import System.FilePath (dropExtension, joinPath, splitPath)
import Witch (into)

-- | Load and typecheck all world descriptions from `worlds/*.world`.
--   Emit a warning for each one which fails to parse or typecheck.
loadWorlds ::
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  EntityMap ->
  m WorldMap
loadWorlds em = do
  res <- throwToWarning @SystemFailure $ getDataDirSafe Worlds "worlds"
  case res of
    Nothing -> return M.empty
    Just dir -> do
      worldFiles <- sendIO $ acquireAllWithExt dir "world"
      ws <- mapM (throwToWarning @SystemFailure . loadWorld dir em) worldFiles
      return . M.fromList . catMaybes $ ws

-- | Load a file containing a world DSL term, throwing an exception if
--   it fails to parse or typecheck.
loadWorld ::
  (Has (Throw SystemFailure) sig m) =>
  FilePath ->
  EntityMap ->
  (FilePath, String) ->
  m (Text, Some (TTerm '[]))
loadWorld dir em (fp, src) = do
  wexp <-
    liftEither . left (AssetNotLoaded (Data Worlds) fp . CanNotParseMegaparsec) $
      runParser parseWExp (into @Text src)
  t <-
    withThrow (AssetNotLoaded (Data Worlds) fp . DoesNotTypecheck . prettyText @CheckErr) $
      runReader em . runReader @WorldMap M.empty $
        infer CNil wexp
  return (into @Text (dropExtension (stripDir dir fp)), t)

-- | Strip a leading directory from a 'FilePath'.
stripDir :: FilePath -> FilePath -> FilePath
stripDir dir fp = joinPath (drop (length (splitPath dir)) (splitPath fp))
