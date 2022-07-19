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
  -- * WorldDescription
  WorldDescription,

  -- * Scenario
  Scenario,

  -- ** Fields
  scenarioName,
  scenarioDescription,
  scenarioGoal,
  scenarioCreative,
  scenarioSeed,
  scenarioEntities,
  scenarioRecipes,
  scenarioWorld,
  scenarioRobots,
  scenarioWin,
  scenarioSolution,
  scenarioStepsPerTick,

  -- * Conversion

  -- mkWorldFun,

  -- * Loading from disk
  loadScenario,
  ScenarioCollection (..),
  scenarioCollectionToList,
  ScenarioItem (..),
  _SISingle,
  scenarioItemName,
  loadScenarios,
) where

import Control.Arrow ((&&&), (***))
import Control.Lens hiding (from, (<.>))
import Control.Monad (filterM, unless, when)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Array
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import GHC.Int (Int64)
import Linear.V2
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtensions, (<.>), (</>))
import Witch (from, into)

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Throw.Either (Throw, runThrow, throwError)

import Data.Vector qualified as V
import Paths_swarm (getDataDir, getDataFileName)
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Robot (TRobot, robotName)
import Swarm.Game.Terrain
import Swarm.Game.World
import Swarm.Game.WorldGen (Seed, findGoodOrigin, testWorld2FromArray)
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (reflow)
import Swarm.Util.Yaml

------------------------------------------------------------
-- Robot map
------------------------------------------------------------

-- | XXX
type RobotMap = Map Text TRobot

-- | XXX
buildRobotMap :: [TRobot] -> RobotMap
buildRobotMap = M.fromList . map (view robotName &&& id)

------------------------------------------------------------
-- Lookup utilities
------------------------------------------------------------

-- XXX
getThing :: String -> (Text -> m -> Maybe a) -> Text -> ParserE m a
getThing thing lkup name = do
  m <- getE
  case lkup name m of
    Nothing -> fail $ "Unknown " <> thing <> " name: " ++ show name
    Just a -> return a

-- XXX
getEntity :: Text -> ParserE EntityMap Entity
getEntity = getThing "entity" lookupEntityName

-- XXX
getRobot :: Text -> ParserE RobotMap TRobot
getRobot = getThing "robot" M.lookup

------------------------------------------------------------
-- World cells
------------------------------------------------------------

data Cell = Cell
  { cellTerrain :: TerrainType
  , cellEntity :: Maybe Entity
  , cellRobot :: Maybe TRobot
  }

instance FromJSONE (EntityMap, RobotMap) Cell where
  -- parseJSONE :: Value -> ParserE (EntityMap, RobotMap) Cell
  -- XXX deduplicate this code?
  parseJSONE = withArrayE "tuple" $ \v -> case V.toList v of
    [t] -> liftE (Cell <$> parseJSON t <*> pure Nothing <*> pure Nothing)
    [t, e] -> do
      terr <- liftE $ parseJSON t
      mName <- liftE $ parseJSON @(Maybe Text) e
      ent <- traverse (localE fst . getEntity) mName
      return (Cell terr ent Nothing)
    [t, e, r] -> do
      terr <- liftE $ parseJSON t
      meName <- liftE $ parseJSON @(Maybe Text) e
      ent <- traverse (localE fst . getEntity) meName
      mrName <- liftE $ parseJSON @(Maybe Text) r
      rob <- traverse (localE snd . getRobot) mrName
      return (Cell terr ent rob)
    _ -> fail "palette entry must be 1, 2, or 3 entries"

------------------------------------------------------------
-- World description
------------------------------------------------------------

-- | A description of a world parsed from a YAML file.  The
--   'mkWorldFun' function is used to turn a 'WorldDescription' into a
--   'WorldFun'.
data WorldDescription = WorldDescription
  { defaultTerrain :: Maybe Cell
  , offsetOrigin :: Bool
  , palette :: WorldPalette
  , ul :: V2 Int64
  , area :: [[Cell]]
  }

instance FromJSONE (EntityMap, RobotMap) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> pure pal
      <*> liftE (v .:? "upperleft" .!= V2 0 0)
      <*> liftE ((v .:? "map" .!= "") >>= paintMap pal)

-- | XXX
paintMap :: MonadFail m => WorldPalette -> Text -> m [[Cell]]
paintMap pal = traverse (traverse toCell . into @String) . T.lines
 where
  toCell c = case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
    Nothing -> fail $ "Char not in world palette: " ++ show c
    Just cell -> return cell

newtype WorldPalette = WorldPalette
  {unPalette :: KeyMap Cell}

instance FromJSONE (EntityMap, RobotMap) WorldPalette where
  parseJSONE = withObjectE "palette" $ fmap WorldPalette . mapM parseJSONE

------------------------------------------------------------
-- Scenario
------------------------------------------------------------

-- | A 'Scenario' contains all the information to describe a
--   scenario.
data Scenario = Scenario
  { _scenarioName :: Text
  , _scenarioDescription :: Text
  , _scenarioGoal :: Maybe [Text]
  , _scenarioCreative :: Bool
  , _scenarioSeed :: Maybe Int
  , _scenarioEntities :: EntityMap
  , _scenarioRecipes :: [Recipe Entity]
  , _scenarioWorld :: WorldDescription
  , _scenarioRobots :: [TRobot]
  , _scenarioWin :: Maybe ProcessedTerm
  , _scenarioSolution :: Maybe ProcessedTerm
  , _scenarioStepsPerTick :: Maybe Int
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''Scenario

instance FromJSONE EntityMap Scenario where
  parseJSONE = withObjectE "scenario" $ \v -> do
    em <- liftE (buildEntityMap <$> (v .:? "entities" .!= []))
    rs <- withE em (v ..: "robots")
    let rsMap = buildRobotMap rs
    Scenario
      <$> liftE (v .: "name")
      <*> liftE (v .:? "description" .!= "")
      <*> liftE ((fmap . fmap . map) reflow (v .:? "goal"))
      <*> liftE (v .:? "creative" .!= False)
      <*> liftE (v .:? "seed")
      <*> pure em
      <*> withE em (v ..:? "recipes" ..!= [])
      <*> localE (,rsMap) (v ..: "world")
      <*> pure rs
      <*> liftE (v .:? "win")
      <*> liftE (v .:? "solution")
      <*> liftE (v .:? "stepsPerTick")

--------------------------------------------------
-- Lenses

-- | The name of the scenario.
scenarioName :: Lens' Scenario Text

-- | A high-level description of the scenario, shown /e.g./ in the
--   menu.
scenarioDescription :: Lens' Scenario Text

-- | An explanation of the goal of the scenario (if any), shown to the
--   player during play.
scenarioGoal :: Lens' Scenario (Maybe [Text])

-- | Whether the scenario should start in creative mode.
scenarioCreative :: Lens' Scenario Bool

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed / prompt the user for the seed.
scenarioSeed :: Lens' Scenario (Maybe Int)

-- | Any custom entities used for this scenario.
scenarioEntities :: Lens' Scenario EntityMap

-- | Any custom recipes used in this scenario.
scenarioRecipes :: Lens' Scenario [Recipe Entity]

-- | The starting world for the scenario.
scenarioWorld :: Lens' Scenario WorldDescription

-- | The starting robots for the scenario.  Note this should
--   include the base.
scenarioRobots :: Lens' Scenario [TRobot]

-- | An optional winning condition for the scenario, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
scenarioWin :: Lens' Scenario (Maybe ProcessedTerm)

-- | An optional solution of the scenario, expressed as a
--   program of type @cmd a@. This is useful for automated
--   testing of the win condition.
scenarioSolution :: Lens' Scenario (Maybe ProcessedTerm)

-- | Optionally, specify the maximum number of steps each robot may
--   take during a single tick.
scenarioStepsPerTick :: Lens' Scenario (Maybe Int)

-- | XXX
mkWorldFun :: WorldDescription -> (Seed -> WorldFun Int Entity)
mkWorldFun wd@(WorldDescription {..}) seed = _
 where
  rs = fromIntegral $ length area
  cs = fromIntegral $ length (head area)

  Coords (ulr, ulc) = locToCoords ul

  arr = listArray ((ulr, ulc), (ulr + rs - 1, ulc + cs - 1)) (concat area)

-- XXX but this is where we have to look up the robots, add them world, etc!
-- So maybe mkWorldFun should have a GameState effect

--   case defaultTerrain of
--     Nothing -> do
--       let arr2 = bimap toEnum (fmap (^. entityName)) <$> arr
--       return $
--         fmap ((lkup em <$>) . first fromEnum)
--           . (if offsetOrigin wd then findGoodOrigin else id)
--           . testWorld2FromArray arr2
--     Just def -> do
--       let defTerrain = (fromEnum *** (>>= (`lookupEntityName` em))) def
--       return $ \_ -> worldFunFromArray arr defTerrain

------------------------------------------------------------
-- Loading scenarios
------------------------------------------------------------

-- | Load a scenario with a given name from disk, given an entity map
--   to use.  This function is used if a specific scenario is
--   requested on the command line.
loadScenario ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  String ->
  EntityMap ->
  m Scenario
loadScenario scenario em = do
  libScenario <- sendIO $ getDataFileName $ "scenarios" </> scenario
  libScenarioExt <- sendIO $ getDataFileName $ "scenarios" </> scenario <.> "yaml"

  mfileName <-
    sendIO $
      listToMaybe <$> filterM doesFileExist [scenario, libScenarioExt, libScenario]

  case mfileName of
    Nothing -> throwError @Text $ "Scenario not found: " <> from @String scenario
    Just fileName -> loadScenarioFile em fileName

-- | A scenario item is either a specific scenario, or a collection of
--   scenarios (*e.g.* the scenarios contained in a subdirectory).
data ScenarioItem = SISingle Scenario | SICollection Text ScenarioCollection

-- | Retrieve the name of a scenario item.
scenarioItemName :: ScenarioItem -> Text
scenarioItemName (SISingle s) = s ^. scenarioName
scenarioItemName (SICollection name _) = name

-- | A scenario collection is a tree of scenarios, keyed by name,
--   together with an optional order.  Invariant: every item in the
--   scOrder exists as a key in the scMap.
data ScenarioCollection = SC
  { scOrder :: Maybe [FilePath]
  , scMap :: Map FilePath ScenarioItem
  }

-- | Convert a scenario collection to a list of scenario items.
scenarioCollectionToList :: ScenarioCollection -> [ScenarioItem]
scenarioCollectionToList (SC Nothing m) = M.elems m
scenarioCollectionToList (SC (Just order) m) = (m M.!) <$> order

-- | Load all the scenarios from the scenarios data directory.
loadScenarios :: (Has (Lift IO) sig m) => EntityMap -> m (Either Text ScenarioCollection)
loadScenarios em = runThrow $ do
  dataDir <- sendIO getDataDir
  loadScenarioDir em (dataDir </> "scenarios")

orderFileName :: FilePath
orderFileName = "00-ORDER.txt"

-- | Recursively load all scenarios from a particular directory, and also load
--   the 00-ORDER file (if any) giving the order for the scenarios.
loadScenarioDir ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  EntityMap ->
  FilePath ->
  m ScenarioCollection
loadScenarioDir em dir = do
  let orderFile = dir </> orderFileName
      dirName = takeBaseName dir
  orderExists <- sendIO $ doesFileExist orderFile
  morder <- case orderExists of
    False -> do
      when (dirName /= "Testing") $
        sendIO . putStrLn $
          "Warning: no " <> orderFileName <> " file found in " <> dirName
            <> ", using alphabetical order"
      return Nothing
    True -> Just . filter (not . null) . lines <$> sendIO (readFile orderFile)
  fs <- sendIO $ keepYamlOrDirectory <$> listDirectory dir

  case morder of
    Just order -> do
      let missing = fs \\ order
          dangling = order \\ fs

      unless (null missing) $
        sendIO . putStr . unlines $
          ( "Warning: while processing " <> (dirName </> orderFileName) <> ": files not listed in "
              <> orderFileName
              <> " will be ignored"
          ) :
          map ("  - " <>) missing

      unless (null dangling) $
        sendIO . putStr . unlines $
          ( "Warning: while processing " <> (dirName </> orderFileName)
              <> ": nonexistent files will be ignored"
          ) :
          map ("  - " <>) dangling
    Nothing -> pure ()

  -- Only keep the files from 00-ORDER.txt that actually exist.
  let morder' = filter (`elem` fs) <$> morder
  SC morder' . M.fromList <$> mapM (\item -> (item,) <$> loadScenarioItem em (dir </> item)) fs
 where
  keepYamlOrDirectory = filter (\f -> takeExtensions f `elem` ["", ".yaml"])

-- | Load a scenario item (either a scenario, or a subdirectory
--   containing a collection of scenarios) from a particular path.
loadScenarioItem ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  EntityMap ->
  FilePath ->
  m ScenarioItem
loadScenarioItem em path = do
  isDir <- sendIO $ doesDirectoryExist path
  let collectionName = into @Text . dropWhile isSpace . takeBaseName $ path
  case isDir of
    True -> SICollection collectionName <$> loadScenarioDir em path
    False -> SISingle <$> loadScenarioFile em path

-- | Load a scenario from a file.  The @Maybe Seed@ argument is a
--   seed provided by the user (either on the command line, or
--   specified through the UI), if any.
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

------------------------------------------------------------
-- Some lenses + prisms
------------------------------------------------------------

makePrisms ''ScenarioItem
