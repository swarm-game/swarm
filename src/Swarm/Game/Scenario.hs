{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
  -- * Objectives
  Objective,
  objectiveGoal,
  objectiveCondition,

  -- * WorldDescription
  Cell (..),
  WorldDescription (..),
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

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Throw.Either (Throw, throwError)
import Control.Lens hiding (from, (<.>))
import Control.Monad (filterM)
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Phase
import Swarm.Game.Recipe
import Swarm.Game.Robot (RRobot, RobotR, TRobot)
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.WorldDescription
import Swarm.Language.Pipeline (ProcessedTerm, processTerm)
import Swarm.Util (getDataFileNameSafe, reflow)
import Swarm.Util.Yaml
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import Witch (from, into)

------------------------------------------------------------
-- Scenario objectives
------------------------------------------------------------

-- | An objective is a condition to be achieved by a player in a
--   scenario.
data Objective' r = Objective
  { _objectiveGoal :: [Text]
  , _objectiveCondition :: ScenarioProgram r
  }
  deriving (Generic)

deriving instance Show (ScenarioProgram r) => Show (Objective' r)
deriving instance Eq (ScenarioProgram r) => Eq (Objective' r)
deriving instance ToJSON (ScenarioProgram r) => ToJSON (Objective' r)

type family ScenarioProgram (phase :: Phase) :: * where
  ScenarioProgram 'Raw = Text
  ScenarioProgram 'Template = ProcessedTerm

makeLensesWith (lensRules & generateSignatures .~ False) ''Objective'

type Objective = Objective' Template

-- | An explanation of the goal of the objective, shown to the player
--   during play.  It is represented as a list of paragraphs.
objectiveGoal :: Lens' (Objective' r) [Text]

-- | A winning condition for the objective, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
objectiveCondition :: Lens' (Objective' r) (ScenarioProgram r)

instance FromJSON (ScenarioProgram r) => FromJSON (Objective' r) where
  parseJSON = withObject "objective" $ \v ->
    Objective
      <$> (fmap . map) reflow (v .:? "goal" .!= [])
      <*> (v .: "condition")

------------------------------------------------------------
-- Scenario
------------------------------------------------------------

-- | A 'Scenario' contains all the information to describe a scenario.
--   The type parameter @r@ is the robot phase XXX
data Scenario' phase = Scenario
  { _scenarioVersion :: Int
  , _scenarioName :: Text
  , _scenarioAuthor :: Maybe Text
  , _scenarioDescription :: Text
  , _scenarioCreative :: Bool
  , _scenarioSeed :: Maybe Int
  , _scenarioEntities :: EntityMap
  , _scenarioRecipes :: [Recipe Entity]
  , _scenarioKnown :: [Text]
  , _scenarioWorld :: WorldDescription
  , _scenarioRobots :: [RobotR phase]
  , _scenarioObjectives :: [Objective' phase]
  , _scenarioSolution :: Maybe (ScenarioProgram phase)
  , _scenarioStepsPerTick :: Maybe Int
  }

deriving instance (Show (ScenarioProgram phase), Show (RobotR phase)) => Show (Scenario' phase)
deriving instance (Eq (ScenarioProgram phase), Eq (RobotR phase)) => Eq (Scenario' phase)

makeLensesWith (lensRules & generateSignatures .~ False) ''Scenario'

type Scenario = Scenario' 'Template

instance FromJSONE EntityMap (Scenario' 'Raw) where
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
        <*> pure em
        <*> v ..:? "recipes" ..!= []
        <*> pure known
        <*> localE (,rsMap) (v ..: "world")
        <*> pure rs
        <*> liftE (v .:? "objectives" .!= [])
        <*> liftE (v .:? "solution")
        <*> liftE (v .:? "stepsPerTick")

resolveScenario :: (Has (Lift IO) sig m, Has (Throw Text) sig m) => Scenario' 'Raw -> m Scenario
resolveScenario = undefined

--  mapM process
-- where
--  process t = case processTerm t of
--    Left err -> throwError err
--    Right Nothing -> throwError @Text "Term was only whitespace"
--    Right (Just pt) -> return pt

--------------------------------------------------
-- Lenses

-- | The version number of the scenario schema.  Currently, this
--   should always be 1, but it is ignored.  In the future, this may
--   be used to convert older formats to newer ones, or simply to
--   print a nice error message when we can't read an older format.
scenarioVersion :: Lens' (Scenario' r) Int

-- | The name of the scenario.
scenarioName :: Lens' (Scenario' r) Text

-- | The author of the scenario.
scenarioAuthor :: Lens' (Scenario' r) (Maybe Text)

-- | A high-level description of the scenario, shown /e.g./ in the
--   menu.
scenarioDescription :: Lens' (Scenario' phase) Text

-- | Whether the scenario should start in creative mode.
scenarioCreative :: Lens' (Scenario' phase) Bool

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed / prompt the user for the seed.
scenarioSeed :: Lens' (Scenario' phase) (Maybe Int)

-- | Any custom entities used for this scenario.
scenarioEntities :: Lens' (Scenario' phase) EntityMap

-- | Any custom recipes used in this scenario.
scenarioRecipes :: Lens' (Scenario' phase) [Recipe Entity]

-- | List of entities that should be considered "known", so robots do
--   not have to scan them.
scenarioKnown :: Lens' (Scenario' phase) [Text]

-- | The starting world for the scenario.
scenarioWorld :: Lens' (Scenario' phase) WorldDescription

-- | The starting robots for the scenario.  Note this should
--   include the base.
scenarioRobots :: Lens' (Scenario' phase) [RobotR phase]

-- | A sequence of objectives for the scenario (if any).
scenarioObjectives :: Lens' (Scenario' phase) [Objective' phase]

-- | An optional solution of the scenario, expressed as a
--   program of type @cmd a@. This is useful for automated
--   testing of the win condition.
scenarioSolution :: Lens' (Scenario' phase) (Maybe (ScenarioProgram phase))

-- | Optionally, specify the maximum number of steps each robot may
--   take during a single tick.
scenarioStepsPerTick :: Lens' (Scenario' phase) (Maybe Int)
------------------------------------------------------------
-- Loading scenarios
------------------------------------------------------------

getScenarioPath :: FilePath -> IO (Maybe FilePath)
getScenarioPath scenario = do
  libScenario <- getDataFileNameSafe $ "scenarios" </> scenario
  libScenarioExt <- getDataFileNameSafe $ "scenarios" </> scenario <.> "yaml"

  let candidates = catMaybes [Just scenario, libScenarioExt, libScenario]
  listToMaybe <$> filterM doesFileExist candidates

-- | Load a scenario with a given name from disk, given an entity map
--   to use.  This function is used if a specific scenario is
--   requested on the command line.
loadScenario ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  String ->
  EntityMap ->
  m (Scenario, FilePath)
loadScenario scenario em = do
  mfileName <- sendIO $ getScenarioPath scenario
  case mfileName of
    Nothing -> throwError @Text $ "Scenario not found: " <> from @String scenario
    Just fileName -> (,fileName) <$> loadScenarioFile em fileName

-- | Load a scenario from a file.
loadScenarioFile ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  EntityMap ->
  FilePath ->
  m Scenario
loadScenarioFile em fileName = do
  res <- sendIO $ decodeFileEitherE em fileName
  case res of
    Left parseExn -> throwError @Text (from @String (prettyPrintParseException parseExn))
    Right c -> resolveScenario c
