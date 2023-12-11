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
  StaticStructureInfo (..),
  staticPlacements,
  structureDefs,

  -- ** Fields
  scenarioVersion,
  scenarioName,
  scenarioAuthor,
  scenarioDescription,
  scenarioCreative,
  scenarioSeed,
  scenarioAttrs,
  scenarioEntities,
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
) where

import Control.Arrow ((&&&))
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw
import Control.Lens hiding (from, (.=), (<.>))
import Control.Monad (filterM, unless, when, (<=<))
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
import Swarm.Game.Entity
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Entity.Cosmetic.Assignment (worldAttributes)
import Swarm.Game.Failure
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
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (Parentage (..))
import Swarm.Game.Scenario.Topography.Placement (Orientation (..), applyOrientationTransform)
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type (SymmetryAnnotatedGrid (..))
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Universe
import Swarm.Game.World.Load (loadWorlds)
import Swarm.Game.World.Typecheck (WorldMap)
import Swarm.Language.Direction (getCoordinateOrientation)
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax (AbsoluteDir (DSouth, DWest), Syntax)
import Swarm.Language.Text.Markdown (Document)
import Swarm.Util (binTuples, commaList, failT, histogram, showT)
import Swarm.Util.Effect (ignoreWarnings, throwToMaybe, withThrow)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.Yaml
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

data StaticStructureInfo = StaticStructureInfo
  { _structureDefs :: [SymmetryAnnotatedGrid (Maybe Cell)]
  , _staticPlacements :: M.Map SubworldName [Structure.LocatedStructure]
  }
  deriving (Show)

makeLensesNoSigs ''StaticStructureInfo

-- | Structure templates that may be auto-recognized when constructed
-- by a robot
structureDefs :: Lens' StaticStructureInfo [SymmetryAnnotatedGrid (Maybe Cell)]

-- | A record of the static placements of structures, so that they can be
-- added to the "recognized" list upon scenario initialization
staticPlacements :: Lens' StaticStructureInfo (M.Map SubworldName [Structure.LocatedStructure])

------------------------------------------------------------
-- Scenario
------------------------------------------------------------

-- | A 'Scenario' contains all the information to describe a
--   scenario.
data Scenario = Scenario
  { _scenarioVersion :: Int
  , _scenarioName :: Text
  , _scenarioAuthor :: Maybe Text
  , _scenarioDescription :: Document Syntax
  , _scenarioCreative :: Bool
  , _scenarioSeed :: Maybe Int
  , _scenarioAttrs :: [CustomAttr]
  , _scenarioEntities :: EntityMap
  , _scenarioCosmetics :: M.Map WorldAttr PreservableColor
  , _scenarioRecipes :: [Recipe Entity]
  , _scenarioKnown :: Set EntityName
  , _scenarioWorlds :: NonEmpty WorldDescription
  , _scenarioNavigation :: Navigation (M.Map SubworldName) Location
  , _scenarioStructures :: StaticStructureInfo
  , _scenarioRobots :: [TRobot]
  , _scenarioObjectives :: [Objective]
  , _scenarioSolution :: Maybe ProcessedTerm
  , _scenarioStepsPerTick :: Maybe Int
  }
  deriving (Show)

makeLensesNoSigs ''Scenario

instance FromJSONE (EntityMap, WorldMap) Scenario where
  parseJSONE = withObjectE "scenario" $ \v -> do
    -- parse custom entities
    emRaw <- liftE (v .:? "entities" .!= [])

    parsedAttrs <- liftE (v .:? "attrs" .!= [])
    let mergedCosmetics = worldAttributes <> M.fromList (mapMaybe toHifiPair parsedAttrs)
        attrsUnion = M.keysSet mergedCosmetics

    case run . runThrow $ validateAttrRefs attrsUnion emRaw of
      Right x -> return x
      Left x -> failT [prettyText @LoadingFailure x]

    em <- case run . runThrow $ buildEntityMap emRaw of
      Right x -> return x
      Left x -> failT [prettyText @LoadingFailure x]

    -- Save the passed in WorldMap for later
    worldMap <- snd <$> getE

    -- Get rid of WorldMap from context locally, and combine EntityMap
    -- with any custom entities parsed above
    localE fst $ withE em $ do
      -- parse 'known' entity names and make sure they exist
      known <- liftE (v .:? "known" .!= mempty)
      em' <- getE
      case filter (isNothing . (`lookupEntityName` em')) known of
        [] -> return ()
        unk -> failT ["Unknown entities in 'known' list:", T.intercalate ", " unk]

      -- parse robots and build RobotMap
      rs <- v ..: "robots"
      let rsMap = buildRobotMap rs

      -- NOTE: These have not been merged with their children yet.
      rootLevelSharedStructures :: Structure.InheritedStructureDefs <-
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
            (sequenceA . (id &&& (Structure.mergeStructures mempty Root . Structure.structure)))
            rootLevelSharedStructures

      let namedGrids = map (\(ns, Structure.MergedStructure s _ _) -> Structure.Grid s <$ ns) mergedStructures

      allWorlds <- localE (worldMap,rootLevelSharedStructures,,rsMap) $ do
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

      Scenario
        <$> liftE (v .: "version")
        <*> liftE (v .: "name")
        <*> liftE (v .:? "author")
        <*> liftE (v .:? "description" .!= "")
        <*> liftE (v .:? "creative" .!= False)
        <*> liftE (v .:? "seed")
        <*> pure parsedAttrs
        <*> pure em
        <*> pure mergedCosmetics
        <*> v ..:? "recipes" ..!= []
        <*> pure (Set.fromList known)
        <*> pure allWorlds
        <*> pure mergedNavigation
        <*> pure structureInfo
        <*> pure rs
        <*> (liftE (v .:? "objectives" .!= []) >>= validateObjectives)
        <*> liftE (v .:? "solution")
        <*> liftE (v .:? "stepsPerTick")

--------------------------------------------------

-- | Warns if any recognition orientations are redundant
-- by rotational symmetry.
-- We can accomplish this by testing only two rotations:
--
-- 1. Rotate 90 degrees. If identical to the original
--    orientation, then has 4-fold symmetry and we don't
--    need to check any other orientations.
--    Warn if more than one recognition orientation was supplied.
-- 2. Rotate 180 degrees.  At best, we may now have
--    2-fold symmetry.
--    Warn if two opposite orientations were supplied.
checkSymmetry ::
  (MonadFail m, Eq a) => Structure.NamedGrid a -> m (SymmetryAnnotatedGrid a)
checkSymmetry ng = do
  case symmetryType of
    Structure.FourFold ->
      when (Set.size suppliedOrientations > 1)
        . failT
        . pure
        $ T.unwords ["Redundant orientations supplied; with four-fold symmetry, just supply 'north'."]
    Structure.TwoFold ->
      unless (null redundantOrientations)
        . failT
        . pure
        $ T.unwords
          [ "Redundant"
          , commaList $ map showT redundantOrientations
          , "orientations supplied with two-fold symmetry."
          ]
     where
      redundantOrientations =
        map fst
          . filter ((> 1) . snd)
          . M.toList
          . histogram
          . map getCoordinateOrientation
          $ Set.toList suppliedOrientations
    _ -> return ()

  return $ SymmetryAnnotatedGrid ng symmetryType
 where
  symmetryType
    | quarterTurnRows == originalRows = Structure.FourFold
    | halfTurnRows == originalRows = Structure.TwoFold
    | otherwise = Structure.NoSymmetry

  quarterTurnRows = applyOrientationTransform (Orientation DWest False) originalRows
  halfTurnRows = applyOrientationTransform (Orientation DSouth False) originalRows

  suppliedOrientations = Structure.recognize ng
  Structure.Grid originalRows = Structure.structure ng

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
scenarioDescription :: Lens' Scenario (Document Syntax)

-- | Whether the scenario should start in creative mode.
scenarioCreative :: Lens' Scenario Bool

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed / prompt the user for the seed.
scenarioSeed :: Lens' Scenario (Maybe Int)

-- | Custom attributes defined in the scenario.
scenarioAttrs :: Lens' Scenario [CustomAttr]

-- | Any custom entities used for this scenario.
scenarioEntities :: Lens' Scenario EntityMap

-- | High-fidelity color map for entities
scenarioCosmetics :: Lens' Scenario (M.Map WorldAttr PreservableColor)

-- | Any custom recipes used in this scenario.
scenarioRecipes :: Lens' Scenario [Recipe Entity]

-- | List of entities that should be considered "known", so robots do
--   not have to scan them.
scenarioKnown :: Lens' Scenario (Set EntityName)

-- | The subworlds of the scenario.
-- The "root" subworld shall always be at the head of the list, by construction.
scenarioWorlds :: Lens' Scenario (NonEmpty WorldDescription)

-- | Information required for structure recognition
scenarioStructures :: Lens' Scenario StaticStructureInfo

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
  EntityMap ->
  WorldMap ->
  m (Scenario, FilePath)
loadScenario scenario em worldMap = do
  mfileName <- getScenarioPath scenario
  fileName <- maybe (throwError $ ScenarioNotFound scenario) return mfileName
  (,fileName) <$> loadScenarioFile em worldMap fileName

-- | Load a scenario from a file.
loadScenarioFile ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  EntityMap ->
  WorldMap ->
  FilePath ->
  m Scenario
loadScenarioFile em worldMap fileName =
  (withThrow adaptError . (liftEither <=< sendIO)) $
    decodeFileEitherE (em, worldMap) fileName
 where
  adaptError = AssetNotLoaded (Data Scenarios) fileName . CanNotParseYaml

loadStandaloneScenario ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  m (Scenario, (WorldMap, EntityMap, [Recipe Entity]))
loadStandaloneScenario fp = do
  entities <- loadEntities
  recipes <- loadRecipes entities
  worlds <- ignoreWarnings @(Seq SystemFailure) $ loadWorlds entities
  scene <- fst <$> loadScenario fp entities worlds
  return (scene, (worlds, entities, recipes))
