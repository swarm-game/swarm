{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility functions
module TestUtil where

import Control.Lens (Ixed (ix), to, (&), (.~), (^.), (^?))
import Control.Monad (void, (<=<))
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Swarm.Effect
import Swarm.Failure (SystemFailure)
import Swarm.Game.CESK
import Swarm.Game.Exception
import Swarm.Game.Land
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete (isActive)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.Step (gameTick, hypotheticalRobot, stepCESK)
import Swarm.Language.Cache (ModuleCache, moduleCache)
import Swarm.Language.Module (Module)
import Swarm.Language.Pipeline (processSource)
import Swarm.Language.Syntax (Phase (Elaborated, Instantiated))
import Swarm.Language.Value
import Swarm.Pretty (prettyText)
import Swarm.Util.Lens
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)
import Witch (into)

eval :: GameState -> Text -> IO (GameState, Robot Instantiated, Either Text (Value, Int))
eval g = either (return . (g,hypotheticalRobot undefined 0,) . Left) (evalPT g) <=< processTerm1

processTerm1 :: Text -> IO (Either Text (Module Elaborated))
processTerm1 txt =
  fmap (first prettyText) . runEff . runErrorNoCallStack @SystemFailure $
    processSource Nothing Nothing txt

evalPT :: GameState -> Module Elaborated -> IO (GameState, Robot Instantiated, Either Text (Value, Int))
evalPT g t = evalCESK g (initMachine t)

evalCESK :: GameState -> CESK -> IO (GameState, Robot Instantiated, Either Text (Value, Int))
evalCESK g cesk =
  runCESK 0 cesk
    & runState r
    & runState (g & creativeMode .~ True)
    & runCacheIO moduleCache
    & runTimeIO
    & runMetricIO
    & runEff
    & fmap orderResult
 where
  r = hypotheticalRobot cesk 0
  orderResult ((res, rr), rg) = (rg, rr, res)

runCESK ::
  ( State (Robot Instantiated) :> es
  , State GameState :> es
  , ModuleCache :> es
  , Time :> es
  , Metric :> es
  ) =>
  Int -> CESK -> Eff es (Either Text (Value, Int))
runCESK _ (Up exn _ []) = Left . flip formatExn exn <$> use (landscape . terrainAndEntities . entityMap)
runCESK !steps cesk = case finalValue cesk of
  Just v -> return (Right (v, steps))
  Nothing -> stepCESK cesk >>= runCESK (steps + 1)

play :: GameState -> Text -> IO (Either Text (), GameState)
play g = either (return . (,g) . Left) playPT <=< processTerm1
 where
  playPT t = runEff . runCacheIO moduleCache . runTimeIO . runMetricIO . runState gs $ playUntilDone (hr ^. robotID)
   where
    cesk = initMachine t
    hr = hypotheticalRobot cesk 0
    hid = hr ^. robotID
    gs =
      g
        & flip execState (zoomRobots $ addRobot hr)
        & runPureEff
        & robotInfo . viewCenterRule .~ VCRobot hid
        & creativeMode .~ True

playUntilDone :: (State GameState :> es, IOE :> es, Metric :> es, Time :> es, ModuleCache :> es) => RID -> Eff es (Either Text ())
playUntilDone rid = do
  w <- use $ robotInfo . robotMap
  case w ^? ix rid . to isActive of
    Just True -> do
      void gameTick
      playUntilDone rid
    Just False -> return $ Right ()
    Nothing -> return $ Left . T.pack $ "The robot with ID " <> show rid <> " is nowhere to be found!"

check :: Text -> (Module Elaborated -> Bool) -> Assertion
check code expect =
  processTerm1 code >>= \case
    Left err -> assertFailure $ "Term processing failed: " ++ into @String err
    Right t -> assertBool "Predicate was false!" (expect t)
