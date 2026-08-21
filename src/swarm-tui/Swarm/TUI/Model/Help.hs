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
) where

import Control.Lens (Lens')
import Swarm.Util.Lens (makeLensesNoSigs)

data HelpState = HelpState
  { _curHelpPage :: Maybe FilePath
  , _helpHistory :: [FilePath] -- XXX make this a zipper, so we can go forward and back?
  }

initHelpState :: HelpState
initHelpState = HelpState {_curHelpPage = Nothing, _helpHistory = []}

makeLensesNoSigs ''HelpState

-- | Lens to access the currently viewed help page, if any.
curHelpPage :: Lens' HelpState (Maybe FilePath)

-- | Lens to access the stack of help browsing history.
helpHistory :: Lens' HelpState [FilePath]
