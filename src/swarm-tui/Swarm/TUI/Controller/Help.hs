-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Functions for manipulating the help system.
module Swarm.TUI.Controller.Help (
  visitHelpPage,
  visitPreviousHelpPage,
  visitNextHelpPage,
  openHelp,
  closeHelp,
  toggleHelp,
) where

import Brick (EventM, zoom)
import Control.Lens (Lens', use, uses, (%=), (.=))
import Control.Monad (when)
import Data.List (uncons)
import Swarm.Game.Achievement.Definitions (CategorizedAchievement (GlobalAchievement), GlobalAchievement (LookedAtAboutScreen))
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model (AppState, Name, playState, progression, scenarioState, uiState)
import Swarm.TUI.Model.Achievements (attainAchievement)
import Swarm.TUI.Model.Help (HelpState, curHelpPage, helpCoHistory, helpHistory)
import Swarm.TUI.Model.UI (uiHelp)

-- | Toggle the help system. If it is currently open, close it, saving
--   the current page to history.  If it is currently closed, open it
--   to the most recently visited page if any, or the index otherwise.
toggleHelp :: EventM Name AppState ()
toggleHelp = do
  curPage <- use $ uiState . uiHelp . curHelpPage
  maybe openHelp (const closeHelp) curPage

-- | Save the current help page (if any) to the help browsing history.
saveCurHelpPage :: EventM Name AppState ()
saveCurHelpPage = do
  curPage <- use $ uiState . uiHelp . curHelpPage
  uiState . uiHelp . helpHistory %= maybe id (:) curPage

-- | Visit a page in the help system, automatically pausing the game
--   and saving browsing history as appropriate.
visitHelpPage :: FilePath -> EventM Name AppState ()
visitHelpPage page = do
  -- Auto-pause if currently playing
  Brick.zoom (playState . scenarioState) ensurePause

  -- Add the currently visited help page (if any) to the history
  saveCurHelpPage

  -- Clear the cohistory, if any --- we are visiting a new page, so we
  -- can no longer return to those pages using "forward"
  uiState . uiHelp . helpCoHistory .= []

  -- Visit the requested page
  uiState . uiHelp . curHelpPage .= Just page

  -- Grant achievement for looking at About page
  when (page == "about.md")
    . Brick.zoom (playState . progression)
    . attainAchievement
    $ GlobalAchievement LookedAtAboutScreen

-- | Open the help system to the most recently visited page, if any,
--   or the index otherwise.  Assumes that the help system was
--   previously closed, i.e. makes no attempt to save the current page
--   to the history.
openHelp :: EventM Name AppState ()
openHelp = do
  hist <- use $ uiState . uiHelp . helpHistory

  case hist of
    [] -> visitHelpPage "index.md"
    _ -> visitPreviousHelpPage

  -- Ensure the game is paused, and open the appropriate page.
  Brick.zoom (playState . scenarioState) ensurePause

-- | Close the help system, saving the currently visited page to the history.
closeHelp :: EventM Name AppState ()
closeHelp = do
  saveCurHelpPage
  Brick.zoom (playState . scenarioState) safeAutoUnpause
  uiState . uiHelp . curHelpPage .= Nothing

-- | Pop the previous page in the help system browsing history (if
--   any) and visit it, pushing the current page onto the cohistory.
--   If there is no previous page, do nothing.
visitPreviousHelpPage :: EventM Name AppState ()
visitPreviousHelpPage = shiftHelpZipper helpCoHistory helpHistory

-- | Move to the next page in the help cohistory (i.e. go foward after
--   previously going back).  If there is no next page, do nothing.
visitNextHelpPage :: EventM Name AppState ()
visitNextHelpPage = shiftHelpZipper helpHistory helpCoHistory

-- | Shift back or forward in the help zipper, by moving the current
--   page (if any) /to/ the first lens, and pulling the new current page
--   /from/ the second (if it is nonempty).
shiftHelpZipper :: Lens' HelpState [FilePath] -> Lens' HelpState [FilePath] -> EventM Name AppState ()
shiftHelpZipper toL fromL = do
  mnext <- uses (uiState . uiHelp . fromL) uncons
  mcur <- use $ uiState . uiHelp . curHelpPage
  case mnext of
    Nothing -> pure ()
    Just (p, from') -> do
      Brick.zoom (playState . scenarioState) ensurePause
      Brick.zoom (uiState . uiHelp) $ do
        curHelpPage .= Just p
        toL %= maybe id (:) mcur
        fromL .= from'
