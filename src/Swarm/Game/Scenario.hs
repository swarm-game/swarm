{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  -- * Objectives
  Objective,
  objectiveGoal,
  objectiveCondition,

  -- * WorldDescription
  Cell (..),
  WorldDescription (..),

  -- * Scenario
  Scenario,

  -- ** Fields
  scenarioName,
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
import Control.Arrow ((&&&))
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Throw.Either (Throw, throwError)
import Control.Lens hiding (from, (<.>))
import Control.Monad (filterM, when)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml as Y
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Linear.V2
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Robot (TRobot, trobotName)
import Swarm.Game.Terrain
import Swarm.Language.Pipeline (ProcessedTerm)
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
data Objective = Objective
  { _objectiveGoal :: [Text]
  , _objectiveCondition :: ProcessedTerm
  }
  deriving (Eq, Show, Generic, ToJSON)

makeLensesWith (lensRules & generateSignatures .~ False) ''Objective

-- | An explanation of the goal of the objective, shown to the player
--   during play.  It is represented as a list of paragraphs.
objectiveGoal :: Lens' Objective [Text]

-- | A winning condition for the objective, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
objectiveCondition :: Lens' Objective ProcessedTerm

instance FromJSON Objective where
  parseJSON = withObject "objective" $ \v ->
    Objective
      <$> (fmap . map) reflow (v .:? "goal" .!= [])
      <*> (v .: "condition")

------------------------------------------------------------
-- Robot map
------------------------------------------------------------

-- | A map from names to robots, used to look up robots in scenario
--   descriptions.
type RobotMap = Map Text TRobot

-- | Create a 'RobotMap' from a list of robot templates.
buildRobotMap :: [TRobot] -> RobotMap
buildRobotMap = M.fromList . map (view trobotName &&& id)

------------------------------------------------------------
-- Lookup utilities
------------------------------------------------------------

-- | Look up a thing by name, throwing a parse error if it is not
--   found.
getThing :: String -> (Text -> m -> Maybe a) -> Text -> ParserE m a
getThing thing lkup name = do
  m <- getE
  case lkup name m of
    Nothing -> fail $ "Unknown " <> thing <> " name: " ++ show name
    Just a -> return a

-- | Look up an entity by name in an 'EntityMap', throwing a parse
--   error if it is not found.
getEntity :: Text -> ParserE EntityMap Entity
getEntity = getThing "entity" lookupEntityName

-- | Look up a robot by name in a 'RobotMap', throwing a parse error
--   if it is not found.
getRobot :: Text -> ParserE RobotMap TRobot
getRobot = getThing "robot" M.lookup

------------------------------------------------------------
-- World cells
------------------------------------------------------------

-- | A single cell in a world map, which contains a terrain value,
--   and optionally an entity and robot.
data Cell = Cell
  { cellTerrain :: TerrainType
  , cellEntity :: Maybe Entity
  , cellRobot :: Maybe TRobot
  }
  deriving (Eq, Show)

-- | Parse a tuple such as @[grass, rock, base]@ into a 'Cell'.  The
--   entity and robot, if present, are immediately looked up and
--   converted into 'Entity' and 'TRobot' values.  If they are not
--   found, a parse error results.
instance FromJSONE (EntityMap, RobotMap) Cell where
  parseJSONE = withArrayE "tuple" $ \v -> do
    let tup = V.toList v
    when (null tup || length tup > 3) $ fail "palette entry must have length 1, 2, or 3"

    terr <- liftE $ parseJSON (head tup)

    ent <- case tup ^? ix 1 of
      Nothing -> return Nothing
      Just e -> do
        meName <- liftE $ parseJSON @(Maybe Text) e
        traverse (localE fst . getEntity) meName

    rob <- case tup ^? ix 2 of
      Nothing -> return Nothing
      Just r -> do
        mrName <- liftE $ parseJSON @(Maybe Text) r
        traverse (localE snd . getRobot) mrName

    return $ Cell terr ent rob

------------------------------------------------------------
-- World description
------------------------------------------------------------

-- | A world palette maps characters to 'Cell' values.
newtype WorldPalette = WorldPalette
  {unPalette :: KeyMap Cell}
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) WorldPalette where
  parseJSONE = withObjectE "palette" $ fmap WorldPalette . mapM parseJSONE

-- | A description of a world parsed from a YAML file.
data WorldDescription = WorldDescription
  { defaultTerrain :: Maybe Cell
  , offsetOrigin :: Bool
  , palette :: WorldPalette
  , ul :: V2 Int64
  , area :: [[Cell]]
  }
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> pure pal
      <*> liftE (v .:? "upperleft" .!= V2 0 0)
      <*> liftE ((v .:? "map" .!= "") >>= paintMap pal)

-- | "Paint" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'Cell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap :: MonadFail m => WorldPalette -> Text -> m [[Cell]]
paintMap pal = traverse (traverse toCell . into @String) . T.lines
 where
  toCell c = case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
    Nothing -> fail $ "Char not in world palette: " ++ show c
    Just cell -> return cell

------------------------------------------------------------
-- Scenario
------------------------------------------------------------

-- | A 'Scenario' contains all the information to describe a
--   scenario.
data Scenario = Scenario
  { _scenarioName :: Text
  , _scenarioDescription :: Text
  , _scenarioCreative :: Bool
  , _scenarioSeed :: Maybe Int
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
        <$> liftE (v .: "name")
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

--------------------------------------------------
-- Lenses

-- | The name of the scenario.
scenarioName :: Lens' Scenario Text

-- | A high-level description of the scenario, shown /e.g./ in the
--   menu.
scenarioDescription :: Lens' Scenario Text

-- | Whether the scenario should start in creative mode.
scenarioCreative :: Lens' Scenario Bool

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed / prompt the user for the seed.
scenarioSeed :: Lens' Scenario (Maybe Int)

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
    Right c -> return c
