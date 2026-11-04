{-# LANGUAGE TemplateHaskell #-}

-- | Help system-specific UI state: whether help is active, the
--   focused page, browsing history, etc.
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Help (
  HelpHistEntry (..),
  helpPage,
  helpPageScroll,
  helpFocusRing,
  HelpState,
  initHelpState,
  curHelpPage,
  helpHistoryBack,
  helpHistoryForward,
  helpLinks,
  linkFocusRing,
) where

import Brick.Focus (FocusRing, focusRing)
import Control.Lens (Lens', (^.))
import Data.Maybe (mapMaybe)
import Swarm.Language.Help (FamilyLinks, HelpPage, familyLinks, helpDoc, nextPage, parentPage, prevPage)
import Swarm.TUI.Model.Name (Name (UILink))
import Swarm.Text.Markdown
import Swarm.Util.Lens (makeLensesNoSigs)

-- | An entry in the help browsing history.  Each page in the history
--   also stores its scroll position and focus ring state, so we can
--   restore them when going back to a previous entry in the history.
data HelpHistEntry = HelpHistEntry
  { _helpPage :: FilePath
  , _helpPageScroll :: Int
  -- ^ The number of lines by which the page has been scrolled down
  -- from the top.  See Note [scroll].
  , _helpFocusRing :: FocusRing Name
  }

-- ~~~~ Note [scroll]
--
-- Storing an absolute line count as a scroll amount does not work
-- properly if the window has been resized since the page was last
-- viewed.  Instead we could consider storing a fraction from 0 to 1
-- and calcuating how many lines to scroll on the fly.  However, this
-- is made complicated by the fact that we have to wait until after
-- the page has been rendered and laid out as a widget to find out how
-- many lines it uses, and then later come back to set the scroll.  We
-- would really rather just set the scroll at the same time that we
-- create the widget.  Because resizing the window in between visits
-- to the same history entry is unlikely, we choose the simpler
-- approach.

makeLensesNoSigs ''HelpHistEntry

-- | The path to the history entry's content.
helpPage :: Lens' HelpHistEntry FilePath

-- | The number of lines the page was scrolled from the top.
helpPageScroll :: Lens' HelpHistEntry Int

-- | The focus ring of links in the page.
helpFocusRing :: Lens' HelpHistEntry (FocusRing Name)

-- | The help state is essentially a slightly fancy list zipper,
--   storing the currently viewed help page, and lists of before and
--   after pages that can be traversed using "back" and "forward"
--   actions.
data HelpState = HelpState
  { _curHelpPage :: Maybe FilePath
  -- ^ The currently viewed help page, if any.  The help system is
  --   actively being displayed iff this is Just.
  , _helpHistoryBack :: [HelpHistEntry]
  -- ^ Previously viewed help pages.
  , _helpHistoryForward :: [HelpHistEntry]
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
helpHistoryBack :: Lens' HelpState [HelpHistEntry]

-- | Lens to access the stack of help browsing forward history.
helpHistoryForward :: Lens' HelpState [HelpHistEntry]

-- | Lens to access the focus ring for links displayed on the current help page.
helpLinks :: Lens' HelpState (FocusRing Name)

-- | Construct a focus ring for all the links found in a document.
linkFocusRing :: HelpPage -> FocusRing Name
linkFocusRing pg = focusRing $ docLinkNames (pg ^. helpDoc) ++ familyLinkNames (pg ^. familyLinks)
 where
  docLinkNames :: Document c -> [Name]
  docLinkNames (Document ps) = concatMap paraLinkNames ps

  paraLinkNames :: Paragraph c -> [Name]
  paraLinkNames = \case
    TOCTree {} -> []
    SimpleParagraph ns -> concatMap nodeLinkNames ns
    ListParagraph _ _ is -> concatMap (concatMap paraLinkNames) is

  nodeLinkNames :: Node c -> [Name]
  nodeLinkNames = \case
    LeafLink tgt _ _ -> [UILink tgt]
    _ -> []

  familyLinkNames :: FamilyLinks -> [Name]
  familyLinkNames fl = map UILink $ mapMaybe (fl ^.) [prevPage, parentPage, nextPage]
