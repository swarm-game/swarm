-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Functions for manipulating the help system.
module Swarm.TUI.Controller.Help (
  visitHelpPage,
  visitPreviousHelpPage,
  visitNextHelpPage,
  visitFamily,
  openHelp,
  closeHelp,
  toggleHelp,
) where

import Brick (EventM, lookupViewport, setTop, vpTop, zoom)
import Brick.Focus (focusRing)
import Control.Lens (Lens', to, traversed, use, uses, view, (%=), (.=), (^.), (^?))
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (uncons)
import Data.Text qualified as T
import Swarm.Game.Achievement.Definitions (CategorizedAchievement (GlobalAchievement), GlobalAchievement (LookedAtAboutScreen))
import Swarm.Game.State.Runtime (helpData)
import Swarm.Language.Help (FamilyLinks, familyLinks)
import Swarm.ResourceLoading (atPath)
import Swarm.TUI.Controller.Util (ensurePause, safeAutoUnpause)
import Swarm.TUI.Model (AppState, Name (HelpViewport), helpScroll, playState, progression, runtimeState, scenarioState, uiState)
import Swarm.TUI.Model.Achievements (attainAchievement)
import Swarm.TUI.Model.Help
import Swarm.TUI.Model.UI (uiHelp)
import Swarm.Text.Markdown.Document (Target, targetText)

-- | Toggle the help system. If it is currently open, close it, saving
--   the current page to history.  If it is currently closed, open it
--   to the most recently visited page if any, or the index otherwise.
toggleHelp :: EventM Name AppState ()
toggleHelp = do
  curPage <- use $ uiState . uiHelp . curHelpPage
  maybe openHelp (const closeHelp) curPage

-- | Build a 'HelpHistEntry' corresponding to the currently viewed
--   help page (including current scroll amount and focus ring
--   state), if any.
getCurHelpEntry :: EventM Name AppState (Maybe HelpHistEntry)
getCurHelpEntry = do
  mcurPage <- use $ uiState . uiHelp . curHelpPage
  mview <- lookupViewport HelpViewport
  linkRing <- use $ uiState . uiHelp . helpLinks

  pure $ do
    curPage <- mcurPage
    v <- mview
    pure $ HelpHistEntry curPage (v ^. vpTop) linkRing

-- | Save the current help page (if any) to the help browsing history.
saveCurHelpPage :: EventM Name AppState ()
saveCurHelpPage = do
  mentry <- getCurHelpEntry
  for_ mentry $ \entry -> uiState . uiHelp . helpHistoryBack %= (entry :)

-- | Visit a page in the help system, automatically pausing the game
--   and saving browsing history as appropriate.
visitHelpPage :: FilePath -> EventM Name AppState ()
visitHelpPage page = do
  -- Auto-pause if currently playing
  Brick.zoom (playState . scenarioState) ensurePause

  -- Add the currently visited help page (if any) to the history
  saveCurHelpPage

  -- Clear the forward history, if any --- we are visiting a new page, so we
  -- can no longer return to those pages using "forward"
  uiState . uiHelp . helpHistoryForward .= []

  -- Visit the requested page
  setHelpPage page

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
  hist <- use $ uiState . uiHelp . helpHistoryBack

  case hist of
    [] -> visitHelpPage "index.md"
    _ -> visitPreviousHelpPage

  -- Ensure the game is paused, and open the appropriate page.
  Brick.zoom (playState . scenarioState) ensurePause

-- | Close the help system, saving the currently visited page to the
--   history (unless the current page is not the main index).
closeHelp :: EventM Name AppState ()
closeHelp = do
  curPage <- use $ uiState . uiHelp . curHelpPage
  when (curPage /= Just "index.md") saveCurHelpPage
  Brick.zoom (playState . scenarioState) safeAutoUnpause
  uiState . uiHelp . curHelpPage .= Nothing

-- | Pop the previous page in the help system browsing history (if
--   any) and visit it, pushing the current page onto the forward
--   history stack.  If there is no previous page, do nothing.
visitPreviousHelpPage :: EventM Name AppState ()
visitPreviousHelpPage = shiftHelpZipper helpHistoryForward helpHistoryBack

-- | Move to the next page in the forward help history (i.e. go foward after
--   previously going back).  If there is no next page, do nothing.
visitNextHelpPage :: EventM Name AppState ()
visitNextHelpPage = shiftHelpZipper helpHistoryBack helpHistoryForward

-- | Shift back or forward in the help zipper, by moving the current
--   page (if any) /to/ the first lens, and pulling the new current page
--   /from/ the second (if it is nonempty).
shiftHelpZipper :: Lens' HelpState [HelpHistEntry] -> Lens' HelpState [HelpHistEntry] -> EventM Name AppState ()
shiftHelpZipper toL fromL = do
  mnext <- uses (uiState . uiHelp . fromL) uncons
  mcur <- getCurHelpEntry
  case mnext of
    Nothing -> pure ()
    Just (entry, from') -> do
      Brick.zoom (playState . scenarioState) ensurePause
      setHelpEntry entry
      Brick.zoom (uiState . uiHelp) $ do
        toL %= maybe id (:) mcur
        fromL .= from'

-- | Set the current help page to the given page, automatically
--   setting up the focus ring for all the links it contains.  Note
--   that this function does not do anything to update the history, so
--   it is not safe to call on its own and should not be exported from
--   this module.
setHelpPage :: FilePath -> EventM Name AppState ()
setHelpPage p = do
  help <- use $ runtimeState . helpData
  let links = maybe (focusRing []) linkFocusRing (help ^? atPath p)
  setHelpEntry $ HelpHistEntry p 0 links

-- | Set the current help page from a history entry, also restoring
--   the focus ring + scroll state.
setHelpEntry :: HelpHistEntry -> EventM Name AppState ()
setHelpEntry entry = do
  -- Set help page itself
  uiState . uiHelp . curHelpPage .= Just (entry ^. helpPage)

  -- Restore scroll
  setTop helpScroll (entry ^. helpPageScroll)

  -- Restore focus ring
  uiState . uiHelp . helpLinks .= (entry ^. helpFocusRing)

-- | Visit a relative help page (previous or next sibling, parent).
visitFamily :: Lens' FamilyLinks (Maybe Target) -> EventM Name AppState ()
visitFamily relative = do
  help <- use $ runtimeState . helpData
  mcur <- use $ uiState . uiHelp . curHelpPage
  for_ mcur $ \cur ->
    for_ (help ^? atPath cur) $ \pg ->
      for_ (pg ^. familyLinks . relative . to (fmap (T.unpack . view targetText))) visitHelpPage

-- XXX what's the right lens-y way to do the above?
