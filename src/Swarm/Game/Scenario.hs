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
import Data.Aeson
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Robot (TRobot)
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Logic
import Swarm.Game.Scenario.Objective.Validation
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.WorldDescription
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (getDataFileNameSafe)
import Swarm.Util.Yaml
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import Witch (from, into)
import Data.BoolExpr qualified as BE

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
  , _scenarioEntities :: EntityMap
  , _scenarioRecipes :: [Recipe Entity]
  , _scenarioKnown :: [Text]
  , _scenarioWorld :: WorldDescription
  , _scenarioRobots :: [TRobot]
  , _scenarioObjectives :: [Objective]
  , _scenarioSolution :: Maybe ProcessedTerm
  , _scenarioStepsPerTick :: Maybe Int
  }
  deriving (Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''Scenario

instance FromJSONE EntityMap Scenario where
  parseJSONE = withObjectE "scenario" $ \v -> do
    -- parse custom entities
    em <- liftE (buildEntityMap <$> (v .:? "entities" .!= []))
    -- extend ambient EntityMap with custom entities

    objectivesRaw <- liftE (v .:? "objectives" .!= [])
    objectives <- validateObjectives objectivesRaw

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
        <*> pure objectives
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
  -- FIXME This is just a rendering experiment:
  sendIO $ Y.encodeFile "foo.yaml" demo
  sendIO $ writeFile "blarg.txt" $ show $ toList demo

  sendIO $ Y.encodeFile "foo2.yaml" $ toBoolExpr demo

  -- let newBoolExpr :: BE.boolTreeToDNF
  --     newBoolExpr = BE.pushNotInwards $ toBoolExpr demo
  sendIO $ Y.encodeFile "foo3.yaml" $ BE.boolTreeToDNF $ toBoolExpr demo

  -- sendIO $ writeFile "foo2.yaml" $ show $ toBoolExpr demo


  sendIO $ Y.encodeFile "demo00input.yaml" demo00input
  sendIO $ Y.encodeFile "demo00.yaml" demo00

  sendIO $ Y.encodeFile "demo01input.yaml" demo01input
  sendIO $ Y.encodeFile "demo01.yaml" demo01


  sendIO $ Y.encodeFile "demo0input.yaml" demo0input
  sendIO $ Y.encodeFile "demo0.yaml" demo0

  sendIO $ Y.encodeFile "demo1input.yaml" demo1input
  sendIO $ Y.encodeFile "demo1.yaml" demo1




  sendIO $ Y.encodeFile "demo2input.yaml" demo2input
  sendIO $ Y.encodeFile "demo2.yaml" demo2

  sendIO $ Y.encodeFile "demo3input.yaml" demo3input
  sendIO $ Y.encodeFile "demo3.yaml" demo3

  sendIO $ Y.encodeFile "demo4input.yaml" demo4input
  sendIO $ Y.encodeFile "demo4.yaml" demo4

  sendIO $ Y.encodeFile "demo5input.yaml" demo5input
  sendIO $ Y.encodeFile "demo5.yaml" demo5


  sendIO $ Y.encodeFile "demo6input.yaml" demo6input
  sendIO $ Y.encodeFile "demo6.yaml" demo6


  sendIO $ Y.encodeFile "demo7input.yaml" demo7input
  sendIO $ Y.encodeFile "demo7.yaml" demo7

  sendIO $ Y.encodeFile "demo8input.yaml" demo8input
  sendIO $ Y.encodeFile "demo8.yaml" demo8

  sendIO $ Y.encodeFile "demo9input.yaml" demo9input
  sendIO $ Y.encodeFile "demo9.yaml" demo9

  res <- sendIO $ decodeFileEitherE em fileName
  case res of
    Left parseExn -> throwError @Text (from @String (prettyPrintParseException parseExn))
    Right c -> return c
 where

  demo00input :: BE.BoolExpr String
  demo00input = BE.BOr BE.BFalse BE.BFalse
  demo00 = BE.boolTreeToDNF demo00input

  demo01input :: BE.BoolExpr String
  demo01input = BE.BOr BE.BFalse BE.BTrue
  demo01 = BE.boolTreeToDNF demo01input

  demo0input :: BE.BoolExpr String
  demo0input = BE.BTrue
  demo0 = BE.boolTreeToDNF demo0input

  demo1input :: BE.BoolExpr String
  demo1input = BE.BFalse
  demo1 = BE.boolTreeToDNF demo1input


  demo2input :: BE.BoolExpr String
  demo2input = BE.BOr BE.BFalse (BE.BConst (BE.Positive "foo"))
  demo2 = BE.boolTreeToDNF demo2input


  demo3input :: BE.BoolExpr String
  demo3input = BE.BOr (BE.BConst (BE.Positive "foo")) BE.BTrue
  demo3 = BE.boolTreeToDNF demo3input

  demo4input :: BE.BoolExpr String
  demo4input = BE.BAnd BE.BFalse (BE.BConst (BE.Positive "foo"))
  demo4 = BE.boolTreeToDNF demo4input

  demo5input :: BE.BoolExpr String
  demo5input = BE.BAnd (BE.BConst (BE.Positive "foo")) BE.BTrue
  demo5 = BE.boolTreeToDNF demo5input



  demo6input :: BE.BoolExpr String
  demo6input = BE.BOr
    (BE.BAnd BE.BFalse (BE.BConst (BE.Positive "foo")))
    (BE.BAnd (BE.BConst (BE.Positive "bar")) BE.BFalse)
  demo6 = BE.boolTreeToDNF demo6input




  demo7input :: BE.BoolExpr String
  demo7input = BE.BOr
    (BE.BAnd (BE.BNot BE.BTrue) (BE.BNot (BE.BNot (BE.BNot (BE.BConst (BE.Positive "foo"))))))
    (BE.BAnd (BE.BConst (BE.Positive "bar")) (BE.BNot (BE.BNot BE.BFalse)))
  demo7 = BE.boolTreeToDNF demo7input


  demo8input :: BE.BoolExpr String
  demo8input = BE.BAnd (BE.BNot (BE.BConst (BE.Positive "foo"))) (BE.BConst (BE.Positive "foo"))
  demo8 = BE.boolTreeToDNF demo8input

  demo9input :: BE.BoolExpr String
  demo9input = BE.BAnd (BE.BConst (BE.Positive "foo")) (BE.BConst (BE.Negative "foo"))
  demo9 = BE.boolTreeToDNF demo9input




  demo :: Prerequisite ObjectiveLabel
  demo =
    And $
      Id "a"
        :| [ Not $ And (Id "e" :| pure (Id "f"))
           , Id "d"
           , Or (Id "b" :| pure (Not $ Id "c"))
           ]
