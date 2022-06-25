{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
  -- * The Scenario type
  Scenario (..),

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

  -- * Loading from disk
  loadScenario,
  ScenarioCollection (..),
  scenarioCollectionToList,
  ScenarioItem (..),
  scenarioItemName,
  loadScenarios,
) where

import Control.Arrow ((***))
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

import Paths_swarm (getDataDir, getDataFileName)
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Robot (URobot)
import Swarm.Game.Terrain
import Swarm.Game.World
import Swarm.Game.WorldGen (Seed, findGoodOrigin, testWorld2FromArray)
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (reflow)
import Swarm.Util.Yaml

-- | A 'Scenario' contains all the information to describe a
--   scenario.
data Scenario = Scenario
  { _scenarioName :: Text
  , _scenarioDescription :: Text
  , _scenarioGoal :: Maybe [Text]
  , _scenarioCreative :: Bool -- Maybe generalize this to a mode enumeration
  , _scenarioSeed :: Maybe Int
  , _scenarioEntities :: EntityMap
  , _scenarioRecipes :: [Recipe Entity]
  , _scenarioWorld :: Seed -> WorldFun Int Entity
  , _scenarioRobots :: [URobot]
  , _scenarioWin :: Maybe ProcessedTerm
  , _scenarioSolution :: Maybe ProcessedTerm
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''Scenario

instance FromJSONE EntityMap Scenario where
  parseJSONE = withObjectE "scenario" $ \v -> do
    em <- liftE (buildEntityMap <$> (v .:? "entities" .!= []))
    Scenario
      <$> liftE (v .: "name")
      <*> liftE (v .:? "description" .!= "")
      <*> liftE ((fmap . fmap . map) reflow (v .:? "goal"))
      <*> liftE (v .:? "creative" .!= False)
      <*> liftE (v .:? "seed")
      <*> pure em
      <*> withE em (v ..:? "recipes" ..!= [])
      <*> withE em (mkWorldFun (v .: "world"))
      <*> withE em (v ..: "robots")
      <*> liftE (v .:? "win")
      <*> liftE (v .:? "solution")

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
scenarioWorld :: Lens' Scenario (Seed -> WorldFun Int Entity)

-- | The starting robots for the scenario.  Note this should
--   include the base.
scenarioRobots :: Lens' Scenario [URobot]

-- | An optional winning condition for the scenario, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
scenarioWin :: Lens' Scenario (Maybe ProcessedTerm)

-- | An optional solution of the scenario, expressed as a
--   program of type @cmd a@. This is useful for automated
--   testing of the win condition.
scenarioSolution :: Lens' Scenario (Maybe ProcessedTerm)

-- | A description of a world parsed from a YAML file.  The
--   'mkWorldFun' function is used to turn a 'WorldDescription' into a
--   'WorldFun'.
data WorldDescription = WorldDescription
  { defaultTerrain :: Maybe (TerrainType, Maybe Text)
  , offsetOrigin :: Bool
  , palette :: WorldPalette
  , ul :: V2 Int64
  , area :: Text
  }

instance FromJSON WorldDescription where
  parseJSON = withObject "world description" $ \v ->
    WorldDescription
      <$> v .:? "default"
      <*> v .:? "offset" .!= False
      <*> v .:? "palette" .!= WorldPalette mempty
      <*> v .:? "upperleft" .!= V2 0 0
      <*> v .:? "map" .!= ""

newtype WorldPalette = WorldPalette
  {unPalette :: KeyMap (TerrainType, Maybe Text)}

instance FromJSON WorldPalette where
  parseJSON = withObject "palette" $ fmap WorldPalette . mapM parseJSON

mkWorldFun :: Parser WorldDescription -> ParserE EntityMap (Seed -> WorldFun Int Entity)
mkWorldFun pwd = E $ \em -> do
  wd <- pwd
  let toEntity :: Char -> Parser (Int, Maybe Entity)
      toEntity c = case KeyMap.lookup (Key.fromString [c]) (unPalette (palette wd)) of
        Nothing -> fail $ "Char not in entity palette: " ++ show c
        Just (t, mt) -> case mt of
          Nothing -> return (fromEnum t, Nothing)
          Just name -> case lookupEntityName name em of
            Nothing -> fail $ "Unknown entity name: " ++ show name
            Just e -> return (fromEnum t, Just e)

      grid = map (into @String) . T.lines $ area wd

      rs = fromIntegral $ length grid
      cs = fromIntegral $ length (head grid)

      Coords (ulr, ulc) = locToCoords (ul wd)

  arr <-
    fmap (listArray ((ulr, ulc), (ulr + rs - 1, ulc + cs - 1)))
      . mapM toEntity
      . concat
      $ grid
  case defaultTerrain wd of
    Nothing -> do
      let arr2 = bimap toEnum (fmap (^. entityName)) <$> arr
      return $
        fmap ((lkup em <$>) . first fromEnum)
          . (if offsetOrigin wd then findGoodOrigin else id)
          . testWorld2FromArray arr2
    Just def -> do
      let defTerrain = (fromEnum *** (>>= (`lookupEntityName` em))) def
      return $ \_ -> worldFunFromArray arr defTerrain
 where
  lkup :: EntityMap -> Maybe Text -> Maybe Entity
  lkup _ Nothing = Nothing
  lkup em (Just t) = lookupEntityName t em

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
    True -> Just . lines <$> sendIO (readFile orderFile)
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
