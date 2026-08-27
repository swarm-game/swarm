{-# LANGUAGE TemplateHaskell #-}

-- | Help system-specific UI state: whether help is active, the
--   focused page, browsing history, etc.
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Help (
  HelpState,
  initHelpState,
  curHelpPage,
  helpHistory,
  helpCoHistory,
) where

import Control.Lens (Lens')
import Swarm.Util.Lens (makeLensesNoSigs)

-- | The help state is essentially a slightly fancy list zipper,
--   storing the currently viewed help page, and lists of before and
--   after pages that can be traversed using "back" and "forward"
--   actions.
data HelpState = HelpState
  { _curHelpPage :: Maybe FilePath
  -- ^ The currently viewed help page, if any.  The help system is
  --   actively being displayed iff this is Just.
  , _helpHistory :: [FilePath]
  -- ^ Previously viewed help pages.
  , _helpCoHistory :: [FilePath]
  -- ^ When the "back" action is used to return to previous pages
  --   from the history, pages get pushed into the cohistory, and
  --   can be returned to via the "forward" action.  The cohistory
  --   is cleared when a new page is visited via any action other
  --   than "forward".
  }

initHelpState :: HelpState
initHelpState = HelpState {_curHelpPage = Nothing, _helpHistory = [], _helpCoHistory = []}

makeLensesNoSigs ''HelpState

-- | Lens to access the currently viewed help page, if any.
curHelpPage :: Lens' HelpState (Maybe FilePath)

-- | Lens to access the stack of help browsing history.
helpHistory :: Lens' HelpState [FilePath]

-- | Lens to access the stack of help browsing cohistory.
helpCoHistory :: Lens' HelpState [FilePath]
