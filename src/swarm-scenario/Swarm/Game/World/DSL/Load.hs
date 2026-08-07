-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading world descriptions from `worlds/*.world`.
module Swarm.Game.World.DSL.Load where

import Control.Arrow (left)
import Data.Map qualified as M
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Swarm.Failure (Asset (..), AssetData (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Game.Land
import Swarm.Game.World.DSL.Parse (parseWExp, runParser)
import Swarm.Game.World.DSL.Typecheck
import Swarm.Language.Syntax.Loc (SrcLoc (..))
import Swarm.Pretty (prettyText)
import Swarm.ResourceLoading (getDataDirThrow)
import Swarm.Util (acquireAllWithExt)
import Swarm.Util.Effect (liftEither, withError)
import System.FilePath (dropExtension, takeFileName)
import Witch (into)

-- | Load and typecheck all world descriptions from `worlds/*.world`.
--   Throw an exception if any fail to parse or typecheck.
loadWorlds ::
  (Error SystemFailure :> es, IOE :> es) =>
  TerrainEntityMaps ->
  Eff es WorldMap
loadWorlds tem = do
  dir <- getDataDirThrow Worlds "worlds"
  worldFiles <- liftIO $ acquireAllWithExt dir "world"
  ws <- mapM (loadWorld tem) worldFiles
  return . M.fromList $ ws

-- | Load a file containing a world DSL term, throwing an exception if
--   it fails to parse or typecheck.
loadWorld ::
  (Error SystemFailure :> es) =>
  TerrainEntityMaps ->
  (FilePath, Text) ->
  Eff es (Text, Some (TTerm '[]))
loadWorld tem (fp, src) = do
  wexp <-
    liftEither . left (AssetNotLoaded (Data Worlds) fp . SystemFailure . CanNotParseMegaparsec) $
      runParser parseWExp src
  t <-
    withError (AssetNotLoaded (Data Worlds) fp . SystemFailure . DoesNotTypecheck NoLoc . prettyText @CheckErr) $
      runReader tem . runReader @WorldMap M.empty $
        infer CNil wexp
  return (into @Text (dropExtension (takeFileName fp)), t)
