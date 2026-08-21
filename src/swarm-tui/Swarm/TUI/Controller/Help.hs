-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
module Swarm.TUI.Controller.Help where

import Brick (EventM)
import Control.Lens ((.=))
import Swarm.TUI.Model (AppState, Name, uiState)
import Swarm.TUI.Model.Help (curHelpPage)
import Swarm.TUI.Model.UI (uiHelp)

-- | XXX
toggleHelp :: EventM Name AppState ()
toggleHelp = undefined

-- XXX deal with pausing, history, etc.

-- | XXX
visitHelpPage :: FilePath -> EventM Name AppState ()
visitHelpPage page = uiState . uiHelp . curHelpPage .= Just page

-- XXX Deal with pausing, etc.

-- | XXX
closeHelp :: EventM Name AppState ()
closeHelp = uiState . uiHelp . curHelpPage .= Nothing

-- | XXX
visitPreviousHelpPage :: EventM Name AppState ()
visitPreviousHelpPage = undefined
