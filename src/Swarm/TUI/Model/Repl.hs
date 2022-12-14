{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Swarm.TUI.Model.Repl (
  -- ** REPL
  REPLHistItem (..),
  replItemText,
  isREPLEntry,
  getREPLEntry,
  REPLHistory,
  replIndex,
  replLength,
  replSeq,
  newREPLHistory,
  addREPLItem,
  restartREPLHistory,
  getLatestREPLHistoryItems,
  moveReplHistIndex,
  getCurrentItemText,
  replIndexIsAtInput,
  TimeDir (..),

  -- ** Prompt utils
  REPLPrompt (..),
  removeEntry,

  -- *** REPL Panel Model
  REPLState,
  ReplControlMode (..),
  replPromptType,
  replPromptEditor,
  replPromptText,
  replValid,
  replLast,
  replType,
  replControlMode,
  replHistory,
  newREPLEditor,

  -- ** Initialization
  initREPLState,
  defaultPrompt,
  lastEntry,
) where

import Brick.Widgets.Edit (Editor, applyEdit, editorText, getEditContents)
import Control.Applicative (Applicative (liftA2))
import Control.Lens hiding (from, (<.>))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as TZ
import Swarm.Language.Types
import Swarm.TUI.Model.Names

------------------------------------------------------------
-- REPL History
------------------------------------------------------------

-- | An item in the REPL history.
data REPLHistItem
  = -- | Something entered by the user.
    REPLEntry Text
  | -- | A response printed by the system.
    REPLOutput Text
  deriving (Eq, Ord, Show, Read)

-- | Useful helper function to only get user input text.
getREPLEntry :: REPLHistItem -> Maybe Text
getREPLEntry = \case
  REPLEntry t -> Just t
  _ -> Nothing

-- | Useful helper function to filter out REPL output.
isREPLEntry :: REPLHistItem -> Bool
isREPLEntry = isJust . getREPLEntry

-- | Get the text of REPL input/output.
replItemText :: REPLHistItem -> Text
replItemText = \case
  REPLEntry t -> t
  REPLOutput t -> t

-- | History of the REPL with indices (0 is first entry) to the current
--   line and to the first entry since loading saved history.
--   We also (ab)use the length of the REPL as the index of current
--   input line, since that number is one past the index of last entry.
data REPLHistory = REPLHistory
  { _replSeq :: Seq REPLHistItem
  , _replIndex :: Int
  , _replStart :: Int
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''REPLHistory

-- | Sequence of REPL inputs and outputs, oldest entry is leftmost.
replSeq :: Lens' REPLHistory (Seq REPLHistItem)

-- | The current index in the REPL history (if the user is going back
--   through the history using up/down keys).
replIndex :: Lens' REPLHistory Int

-- | The index of the first entry since loading saved history.
--
-- It will be set on load and reset on save (happens during exit).
replStart :: Lens' REPLHistory Int

-- | Create new REPL history (i.e. from loaded history file lines).
newREPLHistory :: [REPLHistItem] -> REPLHistory
newREPLHistory xs =
  let s = Seq.fromList xs
   in REPLHistory
        { _replSeq = s
        , _replStart = length s
        , _replIndex = length s
        }

-- | Point the start of REPL history after current last line. See 'replStart'.
restartREPLHistory :: REPLHistory -> REPLHistory
restartREPLHistory h = h & replStart .~ replLength h

-- | Current number lines of the REPL history - (ab)used as index of input buffer.
replLength :: REPLHistory -> Int
replLength = length . _replSeq

-- | Add new REPL input - the index must have been pointing one past
--   the last element already, so we increment it to keep it that way.
addREPLItem :: REPLHistItem -> REPLHistory -> REPLHistory
addREPLItem t h =
  h
    & replSeq %~ (|> t)
    & replIndex .~ 1 + replLength h

-- | Get the latest N items in history, starting with the oldest one.
--
-- This is used to show previous REPL lines in UI, so we need the items
-- sorted in the order they were entered and will be drawn top to bottom.
getLatestREPLHistoryItems :: Int -> REPLHistory -> [REPLHistItem]
getLatestREPLHistoryItems n h = toList latestN
 where
  latestN = Seq.drop oldestIndex $ h ^. replSeq
  oldestIndex = max (h ^. replStart) $ length (h ^. replSeq) - n

data TimeDir = Newer | Older deriving (Eq, Ord, Show)

moveReplHistIndex :: TimeDir -> Text -> REPLHistory -> REPLHistory
moveReplHistIndex d lastEntered history = history & replIndex .~ newIndex
 where
  historyLen = replLength history
  curText = fromMaybe lastEntered $ getCurrentItemText history
  curIndex = history ^. replIndex
  entries = history ^. replSeq
  -- split repl at index
  (olderP, newer) = Seq.splitAt curIndex entries
  -- find first different entry in direction
  notSameEntry = \case
    REPLEntry t -> t /= curText
    _ -> False
  newIndex = case d of
    Newer -> maybe historyLen (curIndex +) $ Seq.findIndexL notSameEntry newer
    Older -> fromMaybe curIndex $ Seq.findIndexR notSameEntry olderP

getCurrentItemText :: REPLHistory -> Maybe Text
getCurrentItemText history = replItemText <$> Seq.lookup (history ^. replIndex) (history ^. replSeq)

replIndexIsAtInput :: REPLHistory -> Bool
replIndexIsAtInput repl = repl ^. replIndex == replLength repl

-- | Given some text,  removes the REPLEntry within REPLHistory which is equal to that.
--   This is used when the user enters in search mode and want to traverse the history.
--   If a command has been used many times, the history will be populated with it causing
--   the effect that search command always finds the same command.
removeEntry :: Text -> REPLHistory -> REPLHistory
removeEntry foundtext hist = hist & replSeq %~ Seq.filter (/= REPLEntry foundtext)

-- | Get the last REPLEntry in REPLHistory matching the given text
lastEntry :: Text -> REPLHistory -> Maybe Text
lastEntry t h =
  case Seq.viewr $ Seq.filter matchEntry $ h ^. replSeq of
    Seq.EmptyR -> Nothing
    _ Seq.:> a -> Just (replItemText a)
 where
  matchesText histItem = t `T.isInfixOf` replItemText histItem
  matchEntry = liftA2 (&&) matchesText isREPLEntry

------------------------------------------------------------
-- REPL
------------------------------------------------------------

-- | This data type tells us how to interpret the text typed
--   by the player at the prompt (which is stored in Editor).
data REPLPrompt
  = -- | Interpret the prompt text as a regular command.
    --   The list is for potential completions, which we can
    --   cycle through by hitting Tab repeatedly
    CmdPrompt [Text]
  | -- | Interpret the prompt text as "search this text in history"
    SearchPrompt REPLHistory

defaultPrompt :: REPLPrompt
defaultPrompt = CmdPrompt []

data ReplControlMode
  = Piloting
  | Typing
  deriving (Enum, Bounded, Eq)

data REPLState = REPLState
  { _replPromptType :: REPLPrompt
  , _replPromptEditor :: Editor Text Name
  , _replValid :: Bool
  , _replLast :: Text
  , _replType :: Maybe Polytype
  , _replControlMode :: ReplControlMode
  , _replHistory :: REPLHistory
  }

newREPLEditor :: Text -> Editor Text Name
newREPLEditor t = applyEdit gotoEnd $ editorText REPLInput (Just 1) t
 where
  ls = T.lines t
  pos = (length ls - 1, T.length (last ls))
  gotoEnd = if null ls then id else TZ.moveCursor pos

initREPLState :: REPLHistory -> REPLState
initREPLState = REPLState defaultPrompt (newREPLEditor "") True "" Nothing Typing

makeLensesWith (lensRules & generateSignatures .~ False) ''REPLState

-- | The way we interpret text typed by the player in the REPL prompt.
replPromptType :: Lens' REPLState REPLPrompt

-- | The prompt where the user can type input at the REPL.
replPromptEditor :: Lens' REPLState (Editor Text Name)

-- | Convinience lens to get text from editor and replace it with new
--   one that has the provided text.
replPromptText :: Lens' REPLState Text
replPromptText = lens g s
 where
  g r = r ^. replPromptEditor . to getEditContents . to T.concat
  s r t = r & replPromptEditor .~ newREPLEditor t

-- | Whether the prompt text is a valid 'Term'.
replValid :: Lens' REPLState Bool

-- | The type of the current REPL input which should be displayed to
--   the user (if any).
replType :: Lens' REPLState (Maybe Polytype)

-- | The last thing the user has typed which isn't part of the history.
--   This is used to restore the repl form after the user visited the history.
replLast :: Lens' REPLState Text

-- | Piloting or Typing mode
replControlMode :: Lens' REPLState ReplControlMode

-- | History of things the user has typed at the REPL, interleaved
--   with outputs the system has generated.
replHistory :: Lens' REPLState REPLHistory
