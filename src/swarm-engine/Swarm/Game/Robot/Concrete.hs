{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent robot context.
module Swarm.Game.Robot.Concrete where

import Control.Lens hiding (Const, contains)
import Data.Aeson qualified as Ae (Key, KeyValue, ToJSON (..), object, (.=))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Linear
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.CESK qualified as C
import Swarm.Game.Display (Display, defaultRobotDisplay, invisible)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Location (Heading, Location)
import Swarm.Game.Robot
import Swarm.Game.Robot.Context
import Swarm.Game.Universe
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax (Syntax)
import Swarm.Language.Text.Markdown (Document)
import Swarm.Language.Value as V
import System.Clock (TimeSpec)

type instance RobotContextMember 'ConcreteRobot = RobotContext

robotContext :: Lens' Robot RobotContext
robotContext = lens _robotContext (\r x -> r {_robotContext = x})

instance ToSample Robot where
  toSamples _ = SD.singleSample sampleBase
   where
    sampleBase :: Robot
    sampleBase =
      mkRobot
        0
        emptyRobotContext
        Nothing
        "base"
        "The starting robot."
        defaultCosmicLocation
        zero
        defaultRobotDisplay
        (C.initMachine [tmQ| move |] mempty C.emptyStore)
        []
        []
        False
        False
        mempty
        0

mkMachine :: Maybe ProcessedTerm -> C.CESK
mkMachine Nothing = C.Out VUnit C.emptyStore []
mkMachine (Just pt) = C.initMachine pt mempty C.emptyStore

-- | Instantiate a robot template to make it into a concrete robot, by
--    providing a robot ID. Concrete robots also require a location;
--    if the robot template didn't have a location already, just set
--    the location to (0,0) by default.  If you want a different location,
--    set it via 'trobotLocation' before calling 'instantiateRobot'.
--
-- If a machine is not supplied (i.e. 'Nothing'), will fallback to any
-- program specified in the template robot.
instantiateRobot :: Maybe C.CESK -> RID -> TRobot -> Robot
instantiateRobot maybeMachine i r =
  r
    { _robotID = i
    , _robotLocation = fromMaybe defaultCosmicLocation $ _robotLocation r
    , _machine = fromMaybe (mkMachine $ _machine r) maybeMachine
    , _robotContext = emptyRobotContext
    }

(.=?) :: (Ae.KeyValue a, Ae.ToJSON v, Eq v) => Ae.Key -> v -> v -> Maybe a
(.=?) n v defaultVal = if defaultVal /= v then Just $ n Ae..= v else Nothing

(.==) :: (Ae.KeyValue a, Ae.ToJSON v) => Ae.Key -> v -> Maybe a
(.==) n v = Just $ n Ae..= v

instance Ae.ToJSON Robot where
  toJSON r =
    Ae.object $
      catMaybes
        [ "id" .== (r ^. robotID)
        , "name" .== (r ^. robotEntity . entityDisplay)
        , "description" .=? (r ^. robotEntity . entityDescription) $ mempty
        , "loc" .== (r ^. robotLocation)
        , "dir" .=? (r ^. robotEntity . entityOrientation) $ zero
        , "display" .=? (r ^. robotDisplay) $ (defaultRobotDisplay & invisible .~ sys)
        , "program" .== (r ^. machine)
        , "devices" .=? (map (^. _2 . entityName) . elems $ r ^. equippedDevices) $ []
        , "inventory" .=? (map (_2 %~ view entityName) . elems $ r ^. robotInventory) $ []
        , "system" .=? sys $ False
        , "heavy" .=? (r ^. robotHeavy) $ False
        , "log" .=? (r ^. robotLog) $ mempty
        , -- debug
          "capabilities" .=? (r ^. robotCapabilities) $ mempty
        , "logUpdated" .=? (r ^. robotLogUpdated) $ False
        , "context" .=? (r ^. robotContext) $ emptyRobotContext
        , "parent" .=? (r ^. robotParentID) $ Nothing
        , "createdAt" .=? (r ^. robotCreatedAt) $ 0
        , "selfDestruct" .=? (r ^. selfDestruct) $ False
        , "activity" .=? (r ^. activityCounts) $ emptyActivityCount
        , "runningAtomic" .=? (r ^. runningAtomic) $ False
        ]
   where
    sys = r ^. systemRobot
