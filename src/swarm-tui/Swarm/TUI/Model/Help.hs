{-# LANGUAGE TemplateHaskell #-}

-- | Help system-specific UI state: whether help is active, the
--   focused page, browsing history, etc.
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Help (
  HelpState,
  initHelpState,
  curHelpPage,
  helpHistoryBack,
  helpHistoryForward,
  helpLinks,
  linkFocusRing,
) where

import Brick.Focus (FocusRing, focusRing)
import Control.Lens (Lens')
import Swarm.TUI.Model.Name (Name (UILink))
import Swarm.Text.Markdown
import Swarm.Util.Lens (makeLensesNoSigs)

-- | The help state is essentially a slightly fancy list zipper,
--   storing the currently viewed help page, and lists of before and
--   after pages that can be traversed using "back" and "forward"
--   actions.
data HelpState = HelpState
  { _curHelpPage :: Maybe FilePath
  -- ^ The currently viewed help page, if any.  The help system is
  --   actively being displayed iff this is Just.
  , _helpHistoryBack :: [FilePath]
  -- ^ Previously viewed help pages.
  , _helpHistoryForward :: [FilePath]
  -- ^ When the "back" action is used to return to previous pages from
  --   the history, pages get pushed into the forward history, and can
  --   be returned to via the "forward" action.  The forward history
  --   is cleared when a new page is visited via any action other than
  --   "forward".
  , _helpLinks :: FocusRing Name
  -- TODO(#2801): this should be generalized to allow cycling through
  -- links in other displayed Markdown documents (e.g. scenario or
  -- entity descriptions), not just in help pages.
  }

initHelpState :: HelpState
initHelpState =
  HelpState
    { _curHelpPage = Nothing
    , _helpHistoryBack = []
    , _helpHistoryForward = []
    , _helpLinks = focusRing []
    }

makeLensesNoSigs ''HelpState

-- | Lens to access the currently viewed help page, if any.
curHelpPage :: Lens' HelpState (Maybe FilePath)

-- | Lens to access the stack of help browsing history.
helpHistoryBack :: Lens' HelpState [FilePath]

-- | Lens to access the stack of help browsing forward history.
helpHistoryForward :: Lens' HelpState [FilePath]

-- | Lens to access the focus ring for links displayed on the current help page.
helpLinks :: Lens' HelpState (FocusRing Name)

-- | Construct a focus ring for all the links found in a document.
linkFocusRing :: Document c -> FocusRing Name
linkFocusRing = focusRing . docLinks
 where
  docLinks :: Document c -> [Name]
  docLinks (Document ps) = concatMap paraLinks ps

  paraLinks :: Paragraph c -> [Name]
  paraLinks = \case
    TOCTree {} -> []
    SimpleParagraph ns -> concatMap nodeLinks ns
    ListParagraph _ _ is -> concatMap (concatMap paraLinks) is

  nodeLinks :: Node c -> [Name]
  nodeLinks = \case
    LeafLink tgt _ _ -> [UILink tgt]
    _ -> []
