{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Standalone worlds
--
-- Scenarios are standalone worlds with specific starting and winning
-- conditions, which can be used both for building interactive
-- tutorials and for standalone puzzles and scenarios.
module Swarm.Game.Scenario (
  -- * Scenario
  Scenario (..),
  ScenarioLandscape (..),
  ScenarioMetadata (ScenarioMetadata),
  staticPlacements,
  structureDefs,

  -- ** Fields
  scenarioMetadata,
  scenarioOperation,
  scenarioLandscape,
  scenarioVersion,
  scenarioName,
  scenarioAuthor,
  scenarioDescription,
  scenarioCreative,
  scenarioSeed,
  scenarioAttrs,
  scenarioTerrainAndEntities,
  scenarioCosmetics,
  scenarioRecipes,
  scenarioKnown,
  scenarioWorlds,
  scenarioNavigation,
  scenarioStructures,
  scenarioRobots,
  scenarioObjectives,
  scenarioSolution,
  scenarioStepsPerTick,

  -- * Loading from disk
  loadScenario,
  loadScenarioFile,
  getScenarioPath,
  loadStandaloneScenario,
  GameStateInputs (..),
  ScenarioInputs (..),

  -- * Utilities
  arbitrateSeed,
) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw
import Control.Lens hiding (from, (.=), (<.>))
import Control.Monad (filterM, forM_, unless, (<=<))
import Data.Aeson
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Failure
import Swarm.Game.Entity
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Entity.Cosmetic.Assignment (worldAttributes)
import Swarm.Game.Land
import Swarm.Game.Location (Location)
import Swarm.Game.Recipe
import Swarm.Game.ResourceLoading (getDataFileNameSafe)
import Swarm.Game.Robot (TRobot, trobotLocation, trobotName)
import Swarm.Game.Scenario.Objective (Objective)
import Swarm.Game.Scenario.Objective.Validation
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Style
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Navigation.Portal
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (Parentage (..))
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly qualified as Assembly
import Swarm.Game.Scenario.Topography.Structure.Named qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static
import Swarm.Game.Scenario.Topography.Structure.Recognition.Symmetry
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Terrain
import Swarm.Game.Universe
import Swarm.Game.World.Gen (Seed)
import Swarm.Game.World.Load (loadWorlds)
import Swarm.Game.World.Typecheck (WorldMap)
import Swarm.Language.Syntax (Syntax, TSyntax)
import Swarm.Language.Text.Markdown (Document)
import Swarm.Pretty (prettyText)
import Swarm.Util (binTuples, commaList, failT, quote)
import Swarm.Util.Effect (ignoreWarnings, throwToMaybe, withThrow)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.Yaml
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import System.Random (randomRIO)

-- * Scenario records

-- | Authorship information about scenario not used at play-time
data ScenarioMetadata = ScenarioMetadata
  { _scenarioVersion :: Int
  , _scenarioName :: Text
  , _scenarioAuthor :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON ScenarioMetadata where
  toEncoding =
    genericToEncoding
      defaultOptions
        { fieldLabelModifier = drop 1 -- drops leading underscore
        }

makeLensesNoSigs ''ScenarioMetadata

-- | The version number of the scenario schema.  Currently, this
--   should always be 1, but it is ignored.  In the future, this may
--   be used to convert older formats to newer ones, or simply to
--   print a nice error message when we can't read an older format.
scenarioVersion :: Lens' ScenarioMetadata Int

-- | The name of the scenario.
scenarioName :: Lens' ScenarioMetadata Text

-- | The author of the scenario.
scenarioAuthor :: Lens' ScenarioMetadata (Maybe Text)

-- | Non-structural gameplay content of the scenario;
-- how it is to be played.
data ScenarioOperation = ScenarioOperation
  { _scenarioCreative :: Bool
  , _scenarioDescription :: Document Syntax
  -- ^ Note: the description is in this record instead of
  -- 'ScenarioMetadata' because it relates to the goals.
  , _scenarioObjectives :: [Objective]
  , _scenarioSolution :: Maybe TSyntax
  , _scenarioRecipes :: [Recipe Entity]
  , _scenarioStepsPerTick :: Maybe Int
  }
  deriving (Show)

makeLensesNoSigs ''ScenarioOperation

-- | A high-level description of the scenario, shown /e.g./ in the
--   menu.
scenarioDescription :: Lens' ScenarioOperation (Document Syntax)

-- | Whether the scenario should start in creative mode.
scenarioCreative :: Lens' ScenarioOperation Bool

-- | Any custom recipes used in this scenario.
scenarioRecipes :: Lens' ScenarioOperation [Recipe Entity]

-- | A sequence of objectives for the scenario (if any).
scenarioObjectives :: Lens' ScenarioOperation [Objective]

-- | An optional solution of the scenario, expressed as a
--   program of type @cmd a@. This is useful for automated
--   testing of the win condition.
scenarioSolution :: Lens' ScenarioOperation (Maybe TSyntax)

-- | Optionally, specify the maximum number of steps each robot may
--   take during a single tick.
scenarioStepsPerTick :: Lens' ScenarioOperation (Maybe Int)

-- | All cosmetic and structural content of the scenario.
data ScenarioLandscape = ScenarioLandscape
  { _scenarioSeed :: Maybe Int
  , _scenarioAttrs :: [CustomAttr]
  , _scenarioTerrainAndEntities :: TerrainEntityMaps
  , _scenarioCosmetics :: M.Map WorldAttr PreservableColor
  , _scenarioKnown :: Set EntityName
  , _scenarioWorlds :: NonEmpty WorldDescription
  , _scenarioNavigation :: Navigation (M.Map SubworldName) Location
  , _scenarioStructures :: StaticStructureInfo Cell
  , _scenarioRobots :: [TRobot]
  }
  deriving (Show)

makeLensesNoSigs ''ScenarioLandscape

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed / prompt the user for the seed.
scenarioSeed :: Lens' ScenarioLandscape (Maybe Int)

-- | Custom attributes defined in the scenario.
scenarioAttrs :: Lens' ScenarioLandscape [CustomAttr]

-- | Any custom terrain and entities used for this scenario,
-- combined with the default system terrain and entities.
scenarioTerrainAndEntities :: Lens' ScenarioLandscape TerrainEntityMaps

-- | High-fidelity color map for entities
scenarioCosmetics :: Lens' ScenarioLandscape (M.Map WorldAttr PreservableColor)

-- | List of entities that should be considered "known", so robots do
--   not have to scan them.
scenarioKnown :: Lens' ScenarioLandscape (Set EntityName)

-- | The subworlds of the scenario.
-- The "root" subworld shall always be at the head of the list, by construction.
scenarioWorlds :: Lens' ScenarioLandscape (NonEmpty WorldDescription)

-- | Information required for structure recognition
scenarioStructures :: Lens' ScenarioLandscape (StaticStructureInfo Cell)

-- | Waypoints and inter-world portals
scenarioNavigation :: Lens' ScenarioLandscape (Navigation (M.Map SubworldName) Location)

-- | The starting robots for the scenario.  Note this should
--   include the base.
scenarioRobots :: Lens' ScenarioLandscape [TRobot]

-- | A 'Scenario' contains all the information to describe a
--   scenario.
data Scenario = Scenario
  { _scenarioMetadata :: ScenarioMetadata
  , _scenarioOperation :: ScenarioOperation
  , _scenarioLandscape :: ScenarioLandscape
  }
  deriving (Show)

makeLensesNoSigs ''Scenario

-- | Authorship information about scenario not used at play-time
scenarioMetadata :: Lens' Scenario ScenarioMetadata

-- | Non-structural gameplay content of the scenario;
-- how it is to be played.
scenarioOperation :: Lens' Scenario ScenarioOperation

-- | All cosmetic and structural content of the scenario.
scenarioLandscape :: Lens' Scenario ScenarioLandscape

-- * Parsing

instance FromJSONE ScenarioInputs Scenario where
  parseJSONE = withObjectE "scenario" $ \v -> do
    -- parse custom terrain
    tmRaw <- liftE (v .:? "terrains" .!= [])

    -- parse custom entities
    emRaw <- liftE (v .:? "entities" .!= [])

    parsedAttrs <- liftE (v .:? "attrs" .!= [])
    let mergedCosmetics = worldAttributes <> M.fromList (mapMaybe toHifiPair parsedAttrs)
        attrsUnion = M.keysSet mergedCosmetics

    validatedTerrainObjects <- runValidation $ validateTerrainAttrRefs attrsUnion tmRaw

    let tm = mkTerrainMap validatedTerrainObjects

    runValidation $ validateEntityAttrRefs attrsUnion emRaw

    em <- runValidation $ buildEntityMap emRaw

    let scenarioSpecificTerrainEntities = TerrainEntityMaps tm em

    -- Save the passed in WorldMap for later
    worldMap <- initWorldMap <$> getE

    -- Get rid of WorldMap from context locally, and combine
    -- the default system TerrainMap and EntityMap
    -- with any custom terrain/entities parsed above
    localE initEntityTerrain $ withE scenarioSpecificTerrainEntities $ do
      -- parse 'known' entity names and make sure they exist
      known <- liftE (v .:? "known" .!= mempty)
      combinedTEM <- getE

      let TerrainEntityMaps _tm emCombined = combinedTEM
      case filter (isNothing . (`lookupEntityName` emCombined)) known of
        [] -> return ()
        unk -> failT ["Unknown entities in 'known' list:", T.intercalate ", " unk]

      -- parse robots and build RobotMap
      rs <- v ..: "robots"
      let rsMap = buildRobotMap rs

      -- NOTE: These have not been merged with their children yet.
      rootLevelSharedStructures :: InheritedStructureDefs <-
        localE (,rsMap) $
          v ..:? "structures" ..!= []

      -- TODO (#1611) This is inefficient; instead, we should
      -- form a DAG of structure references and visit deepest first,
      -- caching in a map as we go.
      -- Then, if a given sub-structure is referenced more than once, we don't
      -- have to re-assemble it.
      --
      -- We should also make use of such a pre-computed map in the
      -- invocation of 'mergeStructures' inside WorldDescription.hs.
      mergedStructures <-
        either (fail . T.unpack) return $
          mapM
            (sequenceA . (id &&& (Assembly.mergeStructures mempty Root . Structure.structure)))
            rootLevelSharedStructures

      let namedGrids = map (\(ns, Structure.MergedStructure (PositionedGrid _ s) _ _) -> s <$ ns) mergedStructures

      allWorlds <- localE (WorldParseDependencies worldMap rootLevelSharedStructures rsMap) $ do
        rootWorld <- v ..: "world"
        subworlds <- v ..:? "subworlds" ..!= []
        return $ rootWorld :| subworlds

      let worldsByName = binTuples $ NE.toList $ NE.map (worldName &&& id) allWorlds
          dupedNames = M.keys $ M.filter ((> 1) . length) worldsByName
      unless (null dupedNames) $
        failT
          [ "Subworld names are not unique:"
          , T.intercalate ", " $ map renderWorldName dupedNames
          ]

      -- Validate robot locations
      forM_ rs $ \r -> forM_ (r ^. trobotLocation) $ \rLoc ->
        unless ((rLoc ^. subworld) `M.member` worldsByName)
          . failT
          $ [ "Robot"
            , quote $ r ^. trobotName
            , "specifies location in nonexistent subworld"
            , renderQuotedWorldName (rLoc ^. subworld) <> "."
            , "Valid subworlds are:"
            , commaList $ map renderQuotedWorldName $ M.keys worldsByName
            ]

      let mergedWaypoints =
            M.fromList $
              map (worldName &&& runIdentity . waypoints . navigation) $
                NE.toList allWorlds

      mergedPortals <-
        validatePortals
          . Navigation mergedWaypoints
          . M.unions
          . map (portals . navigation)
          $ NE.toList allWorlds

      let mergedNavigation = Navigation mergedWaypoints mergedPortals
          recognizableGrids = filter Structure.isRecognizable namedGrids

      symmetryAnnotatedGrids <- mapM checkSymmetry recognizableGrids

      let structureInfo =
            StaticStructureInfo symmetryAnnotatedGrids
              . M.fromList
              . NE.toList
              $ NE.map (worldName &&& placedStructures) allWorlds

      seed <- liftE (v .:? "seed")
      let landscape =
            ScenarioLandscape
              seed
              parsedAttrs
              combinedTEM
              mergedCosmetics
              (Set.fromList known)
              allWorlds
              mergedNavigation
              structureInfo
              rs

      metadata <-
        ScenarioMetadata
          <$> liftE (v .: "version")
          <*> liftE (v .: "name")
          <*> liftE (v .:? "author")

      playInfo <-
        ScenarioOperation
          <$> liftE (v .:? "creative" .!= False)
          <*> liftE (v .:? "description" .!= "")
          <*> (liftE (v .:? "objectives" .!= []) >>= validateObjectives)
          <*> liftE (v .:? "solution")
          <*> localE (view entityMap) (v ..:? "recipes" ..!= [])
          <*> liftE (v .:? "stepsPerTick")

      return $ Scenario metadata playInfo landscape
   where
    runValidation f = case run . runThrow $ f of
      Right x -> return x
      Left x -> failT [prettyText @LoadingFailure x]

-- * Loading scenarios

getScenarioPath ::
  (Has (Lift IO) sig m) =>
  FilePath ->
  m (Maybe FilePath)
getScenarioPath scenario = do
  libScenario <- throwToMaybe @SystemFailure $ getDataFileNameSafe Scenarios $ "scenarios" </> scenario
  libScenarioExt <- throwToMaybe @SystemFailure $ getDataFileNameSafe Scenarios $ "scenarios" </> scenario <.> "yaml"
  let candidates = catMaybes [Just scenario, libScenarioExt, libScenario]
  listToMaybe <$> sendIO (filterM doesFileExist candidates)

-- | Load a scenario with a given name from disk, given an entity map
--   to use.  This function is used if a specific scenario is
--   requested on the command line.
loadScenario ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  ScenarioInputs ->
  m (Scenario, FilePath)
loadScenario scenario scenarioInputs = do
  mfileName <- getScenarioPath scenario
  fileName <- maybe (throwError $ ScenarioNotFound scenario) return mfileName
  (,fileName) <$> loadScenarioFile scenarioInputs fileName

-- | Load a scenario from a file.
loadScenarioFile ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ScenarioInputs ->
  FilePath ->
  m Scenario
loadScenarioFile scenarioInputs fileName =
  (withThrow adaptError . (liftEither <=< sendIO)) $
    decodeFileEitherE scenarioInputs fileName
 where
  adaptError = AssetNotLoaded (Data Scenarios) fileName . CanNotParseYaml

loadStandaloneScenario ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  m (Scenario, GameStateInputs)
loadStandaloneScenario fp = do
  tem <- loadEntitiesAndTerrain
  worlds <- ignoreWarnings @(Seq SystemFailure) $ loadWorlds tem
  let scenarioInputs = ScenarioInputs worlds tem
  recipes <- loadRecipes $ tem ^. entityMap
  scene <- fst <$> loadScenario fp scenarioInputs
  return (scene, GameStateInputs scenarioInputs recipes)

data ScenarioInputs = ScenarioInputs
  { initWorldMap :: WorldMap
  -- ^ A collection of typechecked world DSL terms that are available to
  --   be used in scenario definitions.
  , initEntityTerrain :: TerrainEntityMaps
  -- ^ The standard terrain/entity maps loaded from disk.  Individual scenarios
  --   may define additional terrain/entities which will get added to this map
  --   when loading the scenario.
  }

data GameStateInputs = GameStateInputs
  { gsiScenarioInputs :: ScenarioInputs
  , gsiRecipes :: [Recipe Entity]
  -- ^ The standard list of recipes loaded from disk.  Individual scenarios
  --   may define additional recipes which will get added to this list
  --   when loading the scenario.
  }

-- |
-- Decide on a seed.  In order of preference, we will use:
--   1. seed value provided by the user
--   2. seed value specified in the scenario description
--   3. randomly chosen seed value
arbitrateSeed :: Maybe Seed -> ScenarioLandscape -> IO Seed
arbitrateSeed userSeed sLandscape =
  case userSeed <|> sLandscape ^. scenarioSeed of
    Just s -> return s
    Nothing -> randomRIO (0, maxBound :: Int)
