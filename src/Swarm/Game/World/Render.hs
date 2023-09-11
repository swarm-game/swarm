-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TUI-independent world rendering.
module Swarm.Game.World.Render where

import Control.Effect.Lift (sendIO)
import Swarm.Doc.Gen (loadStandaloneScenario)
import Swarm.TUI.Editor.Util (getMapRectangle)
import Swarm.Util.Effect (simpleErrorHandle)

renderScenarioMap :: FilePath -> IO ()
renderScenarioMap fp = simpleErrorHandle $ do
  myScenario <- loadStandaloneScenario fp

  -- getMapRectangle

  sendIO $ print "Hi there."
