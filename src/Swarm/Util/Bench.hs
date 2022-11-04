{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Swarm.Util.Bench where

import Linear (zero)
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Display (defaultRobotDisplay)
import Swarm.Game.Robot
import Swarm.Language.Context (empty)
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import System.Clock (fromNanoSecs)

-- | The program of a robot that does nothing.
idleProgram :: ProcessedTerm
idleProgram = [tmQ| {} |]

fakeRobot :: Robot
fakeRobot =
  mkRobot
    (-1)
    Nothing
    "hypothesis"
    []
    zero
    zero
    defaultRobotDisplay
    (initMachine idleProgram empty emptyStore)
    []
    []
    True
    False
    (fromNanoSecs 0)
