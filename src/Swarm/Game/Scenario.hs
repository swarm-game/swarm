{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Scenarios are standalone worlds with specific starting and winning
-- conditions, which can be used both for building interactive
-- tutorials and for standalone puzzles and scenarios.
module Swarm.Game.Scenario (
  -- * WorldDescription
  PCell (..),
  Cell,
  PWorldDescription (..),
  WorldDescription,
  IndexedTRobot,

  -- * Scenario
  Scenario (..),

  -- ** Fields
  scenarioVersion,
  scenarioName,
  scenarioAuthor,
  scenarioDescription,
  scenarioCreative,
  scenarioSeed,
  scenarioAttrs,
  scenarioEntities,
  scenarioRecipes,
  scenarioKnown,
  scenarioWorlds,
  scenarioNavigation,
  scenarioRobots,
  scenarioObjectives,
  scenarioSolution,
  scenarioStepsPerTick,

  -- * Loading from disk
  loadScenario,
  loadScenarioFile,
  getScenarioPath,
) where

import Control.Arrow ((&&&))
import Control.Lens hiding (from, (.=), (<.>))
import Control.Monad (filterM)
import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (except)
import Data.Aeson
import Data.Either.Extra (eitherToMaybe, maybeToEither)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Entity
import Swarm.Game.Failure
import Swarm.Game.Failure.Render
import Swarm.Game.Location
import Swarm.Game.Recipe
import Swarm.Game.ResourceLoading (getDataFileNameSafe)
import Swarm.Game.Robot (TRobot)
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Validation
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Style
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Navigation.Portal
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Universe
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (failT)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.Yaml
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import Witch (from)

------------------------------------------------------------
-- Scenario
------------------------------------------------------------

-- | A 'Scenario' contains all the information to describe a
--   scenario.
data Scenario = Scenario
  { _scenarioVersion :: Int
  , _scenarioName :: Text
  , _scenarioAuthor :: Maybe Text
  , _scenarioDescription :: Text
  , _scenarioCreative :: Bool
  , _scenarioSeed :: Maybe Int
  , _scenarioAttrs :: [CustomAttr]
  , _scenarioEntities :: EntityMap
  , _scenarioRecipes :: [Recipe Entity]
  , _scenarioKnown :: [Text]
  , _scenarioWorlds :: NonEmpty WorldDescription
  , _scenarioNavigation :: Navigation (M.Map SubworldName) Location
  , _scenarioRobots :: [TRobot]
  , _scenarioObjectives :: [Objective]
  , _scenarioSolution :: Maybe ProcessedTerm
  , _scenarioStepsPerTick :: Maybe Int
  }
  deriving (Eq, Show)

makeLensesNoSigs ''Scenario

instance FromJSONE EntityMap Scenario where
  parseJSONE = withObjectE "scenario" $ \v -> do
    -- parse custom entities
    emRaw <- liftE (v .:? "entities" .!= [])
    em <- case buildEntityMap emRaw of
      Right x -> return x
      Left x -> failT [x]
    -- extend ambient EntityMap with custom entities

    withE em $ do
      -- parse 'known' entity names and make sure they exist
      known <- liftE (v .:? "known" .!= [])
      em' <- getE
      case filter (isNothing . (`lookupEntityName` em')) known of
        [] -> return ()
        unk -> failT ["Unknown entities in 'known' list:", T.intercalate ", " unk]

      -- parse robots and build RobotMap
      rs <- v ..: "robots"
      let rsMap = buildRobotMap rs

      rootLevelSharedStructures <- localE (,rsMap) $ v ..:? "structures" ..!= []

      allWorlds <- localE (\x -> (rootLevelSharedStructures :: Structure.InheritedStructureDefs, (x, rsMap))) $ do
        rootWorld <- v ..: "world"
        subworlds <- v ..:? "subworlds" ..!= []
        return $ rootWorld :| subworlds

      let mergedWaypoints =
            M.fromList $
              map (worldName &&& runIdentity . waypoints . navigation) $
                NE.toList allWorlds

      mergedPortals <-
        validatePortals
          . Navigation mergedWaypoints
          . M.unions
          $ map (portals . navigation)
          $ NE.toList allWorlds

      let mergedNavigation = Navigation mergedWaypoints mergedPortals

      Scenario
        <$> liftE (v .: "version")
        <*> liftE (v .: "name")
        <*> liftE (v .:? "author")
        <*> liftE (v .:? "description" .!= "")
        <*> liftE (v .:? "creative" .!= False)
        <*> liftE (v .:? "seed")
        <*> liftE (v .:? "attrs" .!= [])
        <*> pure em
        <*> v ..:? "recipes" ..!= []
        <*> pure known
        <*> pure allWorlds
        <*> pure mergedNavigation
        <*> pure rs
        <*> (liftE (v .:? "objectives" .!= []) >>= validateObjectives)
        <*> liftE (v .:? "solution")
        <*> liftE (v .:? "stepsPerTick")

--------------------------------------------------
-- Lenses

-- | The version number of the scenario schema.  Currently, this
--   should always be 1, but it is ignored.  In the future, this may
--   be used to convert older formats to newer ones, or simply to
--   print a nice error message when we can't read an older format.
scenarioVersion :: Lens' Scenario Int

-- | The name of the scenario.
scenarioName :: Lens' Scenario Text

-- | The author of the scenario.
scenarioAuthor :: Lens' Scenario (Maybe Text)

-- | A high-level description of the scenario, shown /e.g./ in the
--   menu.
scenarioDescription :: Lens' Scenario Text

-- | Whether the scenario should start in creative mode.
scenarioCreative :: Lens' Scenario Bool

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed / prompt the user for the seed.
scenarioSeed :: Lens' Scenario (Maybe Int)

-- | Custom attributes defined in the scenario.
scenarioAttrs :: Lens' Scenario [CustomAttr]

-- | Any custom entities used for this scenario.
scenarioEntities :: Lens' Scenario EntityMap

-- | Any custom recipes used in this scenario.
scenarioRecipes :: Lens' Scenario [Recipe Entity]

-- | List of entities that should be considered "known", so robots do
--   not have to scan them.
scenarioKnown :: Lens' Scenario [Text]

-- | The subworlds of the scenario.
scenarioWorlds :: Lens' Scenario (NonEmpty WorldDescription)

-- | Waypoints and inter-world portals
scenarioNavigation :: Lens' Scenario (Navigation (M.Map SubworldName) Location)

-- | The starting robots for the scenario.  Note this should
--   include the base.
scenarioRobots :: Lens' Scenario [TRobot]

-- | A sequence of objectives for the scenario (if any).
scenarioObjectives :: Lens' Scenario [Objective]

-- | An optional solution of the scenario, expressed as a
--   program of type @cmd a@. This is useful for automated
--   testing of the win condition.
scenarioSolution :: Lens' Scenario (Maybe ProcessedTerm)

-- | Optionally, specify the maximum number of steps each robot may
--   take during a single tick.
scenarioStepsPerTick :: Lens' Scenario (Maybe Int)
------------------------------------------------------------
-- Loading scenarios
------------------------------------------------------------

getScenarioPath ::
  (MonadIO m) =>
  FilePath ->
  m (Maybe FilePath)
getScenarioPath scenario = do
  libScenario <- e2m $ getDataFileNameSafe Scenarios $ "scenarios" </> scenario
  libScenarioExt <- e2m $ getDataFileNameSafe Scenarios $ "scenarios" </> scenario <.> "yaml"
  let candidates = catMaybes [Just scenario, libScenarioExt, libScenario]
  listToMaybe <$> liftIO (filterM doesFileExist candidates)
 where
  e2m = fmap eitherToMaybe . runExceptT

-- | Load a scenario with a given name from disk, given an entity map
--   to use.  This function is used if a specific scenario is
--   requested on the command line.
loadScenario ::
  (MonadIO m) =>
  String ->
  EntityMap ->
  ExceptT Text m (Scenario, FilePath)
loadScenario scenario em = do
  mfileName <- liftIO $ getScenarioPath scenario
  fileName <- except $ maybeToEither ("Scenario not found: " <> from @String scenario) mfileName
  s <- withExceptT prettyFailure $ loadScenarioFile em fileName
  return (s, fileName)

-- | Load a scenario from a file.
loadScenarioFile ::
  (MonadIO m) =>
  EntityMap ->
  FilePath ->
  ExceptT SystemFailure m Scenario
loadScenarioFile em fileName =
  withExceptT (AssetNotLoaded (Data Scenarios) fileName . CanNotParse)
    . ExceptT
    . liftIO
    $ decodeFileEitherE em fileName
