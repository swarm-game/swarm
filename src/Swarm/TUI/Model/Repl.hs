{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Repl (
  -- ** REPL
  REPLHistItem (..),
  replItemText,
  isREPLEntry,
  getREPLEntry,
  REPLHistory,
  replIndex,
  replLength,
  replHasExecutedManualInput,
  replSeq,
  newREPLHistory,
  addREPLItem,
  restartREPLHistory,
  getLatestREPLHistoryResult,
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
import Control.Lens hiding (from, (.=), (<.>))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as TZ
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Language.Types
import Swarm.TUI.Model.Name
import Swarm.Util.Lens (makeLensesNoSigs)
import Prelude hiding (Applicative (..))

------------------------------------------------------------
-- REPL History
------------------------------------------------------------

-- | An item in the REPL history.
data REPLHistItem
  = -- | Something entered by the user.
    REPLEntry Text
  | -- | A response printed by the system.
    REPLOutput Text
  | -- | A parsing error from remote entry.
    REPLFailParse Text
  deriving (Eq, Ord, Show, Read)

instance ToSample REPLHistItem where
  toSamples _ = SD.noSamples

instance ToJSON REPLHistItem where
  toJSON e = case e of
    REPLEntry x -> object ["in" .= x]
    REPLOutput x -> object ["out" .= x]
    REPLFailParse x -> object ["fail" .= x]

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
  REPLFailParse t -> t

-- | History of the REPL with indices (0 is first entry) to the current
--   line and to the first entry since loading saved history.
--   We also (ab)use the length of the REPL as the index of current
--   input line, since that number is one past the index of last entry.
data REPLHistory = REPLHistory
  { _replSeq :: Seq REPLHistItem
  , _replIndex :: Int
  , _replStart :: Int
  , _replHasExecutedManualInput :: Bool
  }
  deriving (Show)

makeLensesNoSigs ''REPLHistory

-- | Sequence of REPL inputs and outputs, oldest entry is leftmost.
replSeq :: Lens' REPLHistory (Seq REPLHistItem)

-- | The current index in the REPL history (if the user is going back
--   through the history using up/down keys).
replIndex :: Lens' REPLHistory Int

-- | The index of the first entry since loading saved history.
--
-- It will be set on load and reset on save (happens during exit).
replStart :: Lens' REPLHistory Int

-- | Note: Instead of adding a dedicated field to the REPLHistory record,
-- an early attempt entailed checking for:
--
--    _replIndex > _replStart
--
-- However, executing an initial script causes
-- a "REPLOutput" to be appended to the REPL history,
-- which increments the replIndex, and thus makes
-- the Index greater than the Start even though
-- the player has input not commands into the REPL.
--
-- Therefore, a dedicated boolean is introduced into
-- REPLHistory which simply latches True when the user
-- has input a command.
--
-- An alternative is described here:
-- https://github.com/swarm-game/swarm/pull/974#discussion_r1112380380
replHasExecutedManualInput :: Lens' REPLHistory Bool

-- | Create new REPL history (i.e. from loaded history file lines).
newREPLHistory :: [REPLHistItem] -> REPLHistory
newREPLHistory xs =
  let s = Seq.fromList xs
   in REPLHistory
        { _replSeq = s
        , _replStart = length s
        , _replIndex = length s
        , _replHasExecutedManualInput = False
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

getLatestREPLHistoryResult :: REPLHistory -> Maybe REPLHistItem
getLatestREPLHistoryResult h = do
  i <- Seq.findIndexR isRes hs
  Seq.lookup i hs
 where
  hs = h ^. replSeq
  isRes = \case
    REPLEntry _ -> False
    REPLFailParse _ -> True
    REPLOutput _ -> True

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

-- | What is being done with user input to the REPL panel?
data ReplControlMode
  = -- | The user is typing at the REPL.
    Typing
  | -- | The user is driving the base using piloting mode.
    Piloting
  | -- | A custom user key handler is processing user input.
    Handling
  deriving (Eq, Bounded, Enum)

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
initREPLState hist =
  REPLState
    { _replPromptType = defaultPrompt
    , _replPromptEditor = newREPLEditor ""
    , _replValid = True
    , _replLast = ""
    , _replType = Nothing
    , _replControlMode = Typing
    , _replHistory = hist
    }

makeLensesNoSigs ''REPLState

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

-- | The current REPL control mode, i.e. how user input to the REPL
--   panel is being handled.
replControlMode :: Lens' REPLState ReplControlMode

-- | History of things the user has typed at the REPL, interleaved
--   with outputs the system has generated.
replHistory :: Lens' REPLState REPLHistory
