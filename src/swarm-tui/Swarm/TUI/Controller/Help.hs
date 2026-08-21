-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
module Swarm.TUI.Controller.Help where

import Brick (EventM, zoom)
import Control.Lens (use, (%=), (.=))
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model (AppState, Name, playState, scenarioState, uiState)
import Swarm.TUI.Model.Help (curHelpPage, helpHistory)
import Swarm.TUI.Model.UI (uiHelp)

-- | XXX
toggleHelp :: EventM Name AppState ()
toggleHelp = undefined

-- | Visit a page in the help system, automatically pausing the game
--   and saving browsing history as appropriate.
visitHelpPage :: FilePath -> EventM Name AppState ()
visitHelpPage page = do
  -- Auto-pause if currently playing
  Brick.zoom (playState . scenarioState) ensurePause

  -- Add the currently visited help page (if any) to the history
  curPage <- use $ uiState . uiHelp . curHelpPage
  uiState . uiHelp . helpHistory %= maybe id (:) curPage

  -- Visit the requested page
  uiState . uiHelp . curHelpPage .= Just page

-- XXX Deal with pausing, etc.

-- | XXX
closeHelp :: EventM Name AppState ()
closeHelp = uiState . uiHelp . curHelpPage .= Nothing

-- | Go back to the previous page in the help system browsing history.
visitPreviousHelpPage :: EventM Name AppState ()
visitPreviousHelpPage = undefined
