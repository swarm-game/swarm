{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Load and show Swarm keybindings.
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.KeyBindings (
  initKeyHandlingState,
  KeybindingPrint (..),
  showKeybindings,
  keybindingMeta,
  KeybindingMetadata (..),
) where

import Brick
import Brick.Keybindings as BK
import Control.Lens hiding (from, (<.>))
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Swarm.Failure (Asset (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Pretty (prettyText)
import Swarm.ResourceLoading (getSwarmConfigIniFile)
import Swarm.TUI.Controller.EventHandlers
import Swarm.TUI.Model
import Swarm.TUI.Model.Event (SwarmEvent, defaultSwarmBindings, swarmEvents)

---------------------------------------------------------------------
-- LOADING
---------------------------------------------------------------------

-- See Note [how Swarm event handlers work]

-- | Load keybinding configuration and create key dispatchers.
initKeyHandlingState ::
  (Error SystemFailure :> es, IOE :> es) =>
  Eff es KeyEventHandlingState
initKeyHandlingState = do
  customBindings <- loadKeybindingConfig
  let cfg = newKeyConfig swarmEvents defaultSwarmBindings customBindings
  dispatchers <- createKeyDispatchers cfg
  return $ KeyEventHandlingState cfg dispatchers

loadKeybindingConfig ::
  (Error SystemFailure :> es, IOE :> es) =>
  Eff es [(SwarmEvent, BindingState)]
loadKeybindingConfig = do
  (iniExists, ini) <- liftIO $ getSwarmConfigIniFile False
  if not iniExists
    then return []
    else do
      loadedCustomBindings <- liftIO $ keybindingsFromFile swarmEvents "keybindings" ini
      case loadedCustomBindings of
        Left e -> throwError $ AssetNotLoaded Keybindings ini (SystemFailure . CustomFailure $ T.pack e)
        Right bs -> pure $ fromMaybe [] bs

---------------------------------------------------------------------
-- PRINTING
---------------------------------------------------------------------

data KeybindingPrint = MarkdownPrint | TextPrint | IniPrint
  deriving (Eq, Ord, Show)

-- | Keybinding formatting metadata.
-- 
--  To be used with OverloadedRecordDot, instead of Text tuples.
data KeybindingMetadata = KeyMeta
  { name :: Text
  , keys :: Text
  , description :: Text
  , custom :: Bool
  }

showKeybindings :: KeybindingPrint -> IO Text
showKeybindings kPrint = do
  bindings <- runEff $ runErrorNoCallStack @SystemFailure initKeyHandlingState
  pure $ case bindings of
    Left e -> prettyText e
    Right bs -> showTable kPrint (bs ^. keyConfig) keySections
 where
  showTable = \case
    MarkdownPrint -> keybindingMarkdownTable
    TextPrint -> keybindingTextTable
    IniPrint -> keybindingINI

keySections :: [(Text, [KeyEventHandler SwarmEvent (EventM Name AppState)])]
keySections =
  [ ("Main game (always active)", mainEventHandlers)
  , ("REPL panel", replEventHandlers)
  , ("World view panel", worldEventHandlers)
  , ("Robot inventory panel", robotEventHandlers)
  ]

-- | Keybindings INI file format.
keybindingINI :: Ord k => KeyConfig k -> [(Text, [KeyEventHandler k m])] -> Text
keybindingINI kc sections =
  T.intercalate "\n" $
    "[keybindings]\n"
      : "; Uncomment the assignment and set comma separated list"
      : "; of keybindings or \"unbound\" on the right. See:"
      : "; https://hackage.haskell.org/package/brick/docs/Brick-Keybindings-Parse.html#v:parseBinding\n"
      : concatMap sectionsINI handlersData
 where
  handlersData = map (second $ mapMaybe handlerData) sections
  handlerData h = case kehEventTrigger h of
    ByKey _ -> Nothing
    ByEvent k -> Just (k, handlerDescription $ kehHandler h)
  section s = "\n;;;; " <> s <> "\n"
  sectionsINI (s, hs) = section s : map (keyBindingEventINI kc) hs

-- | Helper function to format one keybinding in the INI format.
-- 
-- >>> let ev = keyEvents [("skip", -1), ("abort", 0), ("continue", 1)]
-- >>> let def = [(-1, [BK.bind 's']), (0, [BK.bind 'a']), (1, [BK.bind 'c'])]
-- >>> let kc = newKeyConfig ev def [(0, Unbound), (1, BindingList [BK.bind 'd'])]
--
-- >>> keyBindingEventINI kc (-1, "Skip selection.")
-- ";; Skip selection.\n; skip = s\n"
-- >>> keyBindingEventINI kc (0, "Abort game.")
-- ";; Abort game.\nabort = unbound\n"
-- >>> keyBindingEventINI kc (1, "Continue game with selection.")
-- ";; Continue game with selection.\ncontinue = d\n"
keyBindingEventINI :: Ord k => KeyConfig k -> (k, Text) -> Text
keyBindingEventINI kc (ev, description) =
  T.unlines
    [ ";; " <> hMeta.description
    , commentDefault <> hMeta.name <> " = " <> hMeta.keys
    ]
 where
  commentDefault = if hMeta.custom then "" else "; "
  hMeta = keybindingMeta' kc (ev, description)

-- | Keybinding metadata used in TUI.
keybindingMeta :: Ord k => KeyConfig k -> KeyEventHandler k m -> KeybindingMetadata
keybindingMeta kc keh = case kehEventTrigger keh of
    ByKey b -> KeyMeta {name = "(non-customizable key)", keys = ppBinding b, description = desc, custom = False }
    ByEvent ev -> keybindingMeta' kc (ev, desc)
 where
  desc = handlerDescription $ kehHandler keh

-- | Common helper function to get keybinding formatting metadata.
keybindingMeta' :: Ord k => KeyConfig k -> (k, Text) -> KeybindingMetadata
keybindingMeta' kc (ev, desc) = case lookupKeyConfigBindings kc ev of
  Nothing | null defaultBind -> unboundResult
  Nothing -> KeyMeta {name = name, keys = ppBindings defaultBind, description = desc, custom = False}
  Just Unbound -> unboundResult
  Just (BindingList []) -> unboundResult
  Just (BindingList bs) -> KeyMeta {name = name, keys = ppBindings bs, description = desc, custom = True}
 where
  unboundResult = KeyMeta {name = name, keys = "unbound", description = desc, custom = not $ null defaultBind}
  defaultBind = allDefaultBindings kc ev
  ppBindings = T.intercalate "," . fmap ppBinding
  name = fromMaybe "(unnamed)" $ keyEventName (keyConfigEvents kc) ev
