{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Swarm.Game.Scenario
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
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
  Scenario,

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
  scenarioWorld,
  scenarioRobots,
  scenarioObjectives,
  scenarioSolution,
  scenarioStepsPerTick,

  -- * Loading from disk
  loadScenario,
  loadScenarioFile,
  getScenarioPath,
) where

import Control.Lens hiding (from, (<.>))
import Control.Monad (filterM)
import Control.Monad.Except (ExceptT (..), MonadIO, liftIO, runExceptT, withExceptT)
import Control.Monad.Trans.Except (except)
import Data.Aeson
import Data.Either.Extra (eitherToMaybe, maybeToEither)
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Robot (TRobot)
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Validation
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Style
import Swarm.Game.Scenario.WorldDescription
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.TUI.Model.Failure
import Swarm.TUI.Model.FailureRender
import Swarm.Util.GameData (getDataFileNameSafe)
import Swarm.Util.Yaml
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import Witch (from, into)

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
  , _scenarioWorld :: WorldDescription
  , _scenarioRobots :: [TRobot]
  , _scenarioObjectives :: [Objective]
  , _scenarioSolution :: Maybe ProcessedTerm
  , _scenarioStepsPerTick :: Maybe Int
  }
  deriving (Eq, Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''Scenario

instance FromJSONE EntityMap Scenario where
  parseJSONE = withObjectE "scenario" $ \v -> do
    -- parse custom entities
    em <- liftE (buildEntityMap <$> (v .:? "entities" .!= []))
    -- extend ambient EntityMap with custom entities

    withE em $ do
      -- parse 'known' entity names and make sure they exist
      known <- liftE (v .:? "known" .!= [])
      em' <- getE
      case filter (isNothing . (`lookupEntityName` em')) known of
        [] -> return ()
        unk ->
          fail . into @String $
            "Unknown entities in 'known' list: " <> T.intercalate ", " unk

      -- parse robots and build RobotMap
      rs <- v ..: "robots"
      let rsMap = buildRobotMap rs

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
        <*> localE (,rsMap) (v ..: "world")
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

-- | The starting world for the scenario.
scenarioWorld :: Lens' Scenario WorldDescription

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
  MonadIO m =>
  FilePath ->
  m (Maybe FilePath)
getScenarioPath scenario = do
  libScenario <- e2m $ getDataFileNameSafe $ "scenarios" </> scenario
  libScenarioExt <- e2m $ getDataFileNameSafe $ "scenarios" </> scenario <.> "yaml"
  let candidates = catMaybes [Just scenario, libScenarioExt, libScenario]
  listToMaybe <$> liftIO (filterM doesFileExist candidates)
 where
  e2m = fmap eitherToMaybe . runExceptT

-- | Load a scenario with a given name from disk, given an entity map
--   to use.  This function is used if a specific scenario is
--   requested on the command line.
loadScenario ::
  MonadIO m =>
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
  MonadIO m =>
  EntityMap ->
  FilePath ->
  ExceptT SystemFailure m Scenario
loadScenarioFile em fileName = do
  withExceptT (AssetNotLoaded (Data Scenarios) . PathLoadFailure fileName . CanNotParse) $
    ExceptT $
      liftIO $
        decodeFileEitherE em fileName
