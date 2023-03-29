{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Analyze pedagogical soundness of the tutorials.
module Swarm.Docs.Pedagogy where

import Control.Lens (universe, view)
import Control.Monad.Except (ExceptT (..), liftIO)
import Data.Char (isLetter, toLower)
import Data.List.Split (wordsBy)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Swarm.Docs.Util
import Swarm.Game.Entity (loadEntities)
import Swarm.Game.Scenario (Scenario, scenarioObjectives, scenarioSolution)
import Swarm.Game.Scenario.Objective (objectiveGoal)
import Swarm.Game.ScenarioInfo (ScenarioInfoPair, flatten, loadScenariosWithWarnings, scenarioCollectionToList, scenarioPath)
import Swarm.Language.Module (Module (..))
import Swarm.Language.Pipeline (ProcessedTerm (..))
import Swarm.Language.Syntax
import Swarm.Language.Types (Polytype)
import Swarm.TUI.Controller (getTutorials)

data TutorialInfo = TutorialInfo
  { scenarioPair :: ScenarioInfoPair
  , solutionCommands :: Set Const
  , descriptionCommands :: Set Const
  }

-- ----------------------------------------------------------------------------
-- GENERATE TUTORIAL PROGRESSION
-- ----------------------------------------------------------------------------

extractCommandUsages :: ScenarioInfoPair -> TutorialInfo
extractCommandUsages siPair@(s, _si) =
  TutorialInfo siPair solnCommands $ getDescCommands s
 where
  solnCommands = S.fromList $ maybe [] getCommands maybeSoln
  maybeSoln = view scenarioSolution s

getDescCommands :: Scenario -> Set Const
getDescCommands s =
  S.fromList $ mapMaybe (`M.lookup` txtLookups) allWords
 where
  goalTextParagraphs = concatMap (view objectiveGoal) $ view scenarioObjectives s
  allWords = concatMap (wordsBy (not . isLetter) . T.unpack . T.toLower) goalTextParagraphs

  txtLookups = M.fromList $ map (\x -> (map toLower $ show x, x)) allConst

getCommands :: ProcessedTerm -> [Const]
getCommands (ProcessedTerm (Module stx _) _ _) =
  mapMaybe isConst nodelist
 where
  nodelist :: [Syntax' Polytype]
  nodelist = universe stx
  isConst (Syntax' _ t _) = case t of
    TConst c -> Just c
    _ -> Nothing

renderUsages :: TutorialInfo -> String
renderUsages (TutorialInfo (_s, si) sCmds dCmds) =
  unlines $
    firstLine
      : "================"
      : (["From solution code:", "----------------"] <> solnCmds <> ["", "From description:", "----------------"] <> descCmds)
 where
  solnCmds = map show $ S.toList sCmds
  descCmds = map show $ S.toList dCmds
  firstLine = view scenarioPath si <> ":"

generateTutorialProgression :: IO String
generateTutorialProgression = simpleErrorHandle $ do
  entities <- ExceptT loadEntities
  (_, loadedScenarios) <- liftIO $ loadScenariosWithWarnings entities
  let orderedTutorials =
        concatMap flatten $
          scenarioCollectionToList $
            getTutorials loadedScenarios

  return $ unlines $ map (renderUsages . extractCommandUsages) orderedTutorials
