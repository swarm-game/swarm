{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.KeyBindings (
  initKeyHandlingState,
  KeybindingPrint (..),
  showKeybindings,
  handlerNameKeysDescription,
) where

import Brick
import Brick.Keybindings as BK
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens hiding (from, (<.>))
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Failure (Asset (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Game.ResourceLoading (getSwarmConfigIniFile)
import Swarm.Language.Pretty (prettyText)
import Swarm.TUI.Controller.EventHandlers
import Swarm.TUI.Model
import Swarm.TUI.Model.Event (SwarmEvent, defaultSwarmBindings, swarmEvents)

loadKeybindingConfig ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  m [(SwarmEvent, BindingState)]
loadKeybindingConfig = do
  (iniExists, ini) <- sendIO getSwarmConfigIniFile
  if not iniExists
    then return []
    else do
      loadedCustomBindings <- sendIO $ keybindingsFromFile swarmEvents "keybindings" ini
      case loadedCustomBindings of
        Left e -> throwError $ AssetNotLoaded Keybindings ini (CustomMessage $ T.pack e)
        Right bs -> pure $ fromMaybe [] bs

initKeyHandlingState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  m KeyEventHandlingState
initKeyHandlingState = do
  customBindings <- loadKeybindingConfig
  let cfg = newKeyConfig swarmEvents defaultSwarmBindings customBindings
  dispatchers <- createKeyDispatchers cfg
  return $ KeyEventHandlingState cfg dispatchers

data KeybindingPrint = MarkdownPrint | TextPrint | IniPrint
  deriving (Eq, Ord, Show)

showKeybindings :: KeybindingPrint -> IO Text
showKeybindings kPrint = do
  bindings <- runM $ runThrow @SystemFailure initKeyHandlingState
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
  , ("REPL panel ", replEventHandlers)
  , ("World view panel", worldEventHandlers)
  , ("Robot inventory panel", robotEventHandlers)
  ]

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

keyBindingEventINI :: Ord k => KeyConfig k -> (k, Text) -> Text
keyBindingEventINI kc (ev, description) =
  T.unlines
    [ ";; " <> description
    , commentDefault <> name <> " = " <> bindingList
    ]
 where
  commentDefault = if custom then "" else "; "
  (custom, bindingList) = case lookupKeyConfigBindings kc ev of
    Just Unbound -> (True, "unbound")
    Just (BindingList bs) -> (True, listBindings bs)
    Nothing ->
      ( False
      , if null (allDefaultBindings kc ev)
          then "unbound"
          else listBindings $ allDefaultBindings kc ev
      )
  listBindings = T.intercalate "," . fmap ppBinding
  name = case keyEventName (keyConfigEvents kc) ev of
    Just n -> n
    Nothing -> error $ "unnamed event: " <> T.unpack description

handlerNameKeysDescription :: Ord k => KeyConfig k -> KeyEventHandler k m -> (Text, Text, Text)
handlerNameKeysDescription kc keh = (name, keys, desc)
 where
  desc = handlerDescription $ kehHandler keh
  (name, keys) = case kehEventTrigger keh of
    ByKey b -> ("(non-customizable key)", ppBinding b)
    ByEvent ev ->
      let name' = fromMaybe "(unnamed)" $ keyEventName (keyConfigEvents kc) ev
       in case lookupKeyConfigBindings kc ev of
            Nothing ->
              if not (null (allDefaultBindings kc ev))
                then (name', T.intercalate "," $ ppBinding <$> allDefaultBindings kc ev)
                else (name', "unbound")
            Just Unbound ->
              (name', "unbound")
            Just (BindingList bs) ->
              let result =
                    if not (null bs)
                      then T.intercalate "," $ ppBinding <$> bs
                      else "unbound"
               in (name', result)
