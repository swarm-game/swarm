{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering of the scenario launch configuration dialog.
module Swarm.TUI.Launch.View where

import Brick
import Brick.Focus
import Brick.Forms qualified as BF
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Edit
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.FileBrowser qualified as FB
import Control.Exception qualified as E
import Control.Lens
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Scenario (scenarioSeed)
import Swarm.Game.Scenario.Status (ParameterizableLaunchParams (..))
import Swarm.Game.State (getRunCodePath)
import Swarm.TUI.Attr
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Util (EllipsisSide (Beginning), withEllipsis)
import Swarm.Util (brackets, parens)

drawFileBrowser :: FB.FileBrowser Name -> Widget Name
drawFileBrowser b =
  centerLayer $ hLimit 50 $ ui <=> help
 where
  ui =
    vLimit 15 $
      borderWithLabel (txt "Choose a file") $
        FB.renderFileBrowser True b

  footerRows =
    map
      (withDefAttr dimAttr . hCenter . txt)
      [ "Up/Down: navigate"
      , "/: search, Ctrl-C or Esc: cancel search"
      , "Enter: change directory or select file"
      , "Esc: quit"
      ]

  help =
    padTop (Pad 1) $
      vBox $
        [ case FB.fileBrowserException b of
            Nothing -> emptyWidget
            Just e ->
              hCenter
                . withDefAttr BF.invalidFormInputAttr
                . txt
                . T.pack
                $ E.displayException e
        ]
          <> footerRows

optionDescription :: ScenarioConfigPanelFocusable -> Maybe Text
optionDescription = \case
  SeedSelector -> Just "Leaving this field blank will use the default seed for the scenario."
  ScriptSelector -> Just "Selecting a script to be run upon start permits eligibility for code size scoring."
  StartGameButton -> Nothing

drawLaunchConfigPanel :: LaunchOptions -> [Widget Name]
drawLaunchConfigPanel (LaunchOptions lc launchParams) =
  addFileBrowser [panelWidget]
 where
  validatedOptions = toValidatedParams launchParams
  LaunchControls (FileBrowserControl fb _ isFbDisplayed) seedEditor ring displayedFor = lc
  addFileBrowser =
    if isFbDisplayed
      then (drawFileBrowser fb :)
      else id

  getFocusedConfigPanel :: Maybe ScenarioConfigPanelFocusable
  getFocusedConfigPanel = case focusGetCurrent ring of
    Just (ScenarioConfigControl (ScenarioConfigPanelControl x)) -> Just x
    _ -> Nothing

  isFocused = (== getFocusedConfigPanel) . Just

  highlightIfFocused x =
    if isFocused x
      then withDefAttr highlightAttr
      else id

  mkButton name label =
    clickable (ScenarioConfigControl $ ScenarioConfigPanelControl name)
      . highlightIfFocused name
      . withAttr boldAttr
      $ txt label

  mkSeedEditorWidget =
    hLimit 10 $
      overrideAttr E.editFocusedAttr customEditFocusedAttr $
        renderEditor (txt . mconcat) (isFocused SeedSelector) seedEditor
  seedEntryWidget = case seedVal launchParams of
    Left _ -> mkSeedEditorWidget
    Right x -> mkSeedEntryWidget x

  scenarioSeedText = maybe "random" show $ view scenarioSeed . fst =<< displayedFor
  mkSeedEntryWidget seedEntryContent =
    if isFocused SeedSelector
      then mkSeedEditorWidget
      else case seedEntryContent of
        Just x -> str $ show x
        Nothing ->
          withDefAttr dimAttr $
            txt $
              T.unwords
                [ "scenario default"
                , parens $ T.pack scenarioSeedText
                ]

  unspecifiedFileMessage =
    if isFocused ScriptSelector
      then str "<[Enter] to select>"
      else withDefAttr dimAttr $ str "<none>"

  fileEntryWidget = case initialCode launchParams of
    Left _ -> str "<invalid>"
    Right maybeFilepath ->
      maybe
        unspecifiedFileMessage
        (withEllipsis Beginning . T.pack)
        (getRunCodePath =<< maybeFilepath)

  panelWidget =
    centerLayer
      . borderWithLabel (str " Configure scenario launch ")
      . hLimit 60
      . padAll 1
      $ vBox widgetMembers
   where
    startButton =
      hCenter . mkButton StartGameButton $
        T.unwords
          [ ">>"
          , "Launch with these settings"
          , "<<"
          ]

    widgetMembers =
      [ controlsBox
      , infoBox
      , if isRight validatedOptions then startButton else emptyWidget
      ]

    formatInfo header content =
      hBox
        [ padLeft (Pad 6) . withAttr boldAttr . txt $ brackets header
        , padLeft (Pad 1) $ txtWrap content
        ]

    infoContent = case validatedOptions of
      Left errmsg -> withDefAttr BF.invalidFormInputAttr $ formatInfo "Error" errmsg
      Right _ -> case optionDescription =<< getFocusedConfigPanel of
        Just desc -> withDefAttr dimAttr $ formatInfo "Info" desc
        Nothing -> str " "

    infoBox =
      vLimit 4
        . padBottom Max
        . padRight (Pad 2)
        $ infoContent

    padControl widgetName label widgetObj =
      padBottom (Pad 1) $
        padLeft (Pad 2) $
          hBox
            [ mkButton widgetName (label <> ": ")
            , widgetObj
            ]

    controlsBox =
      vBox
        [ padControl ScriptSelector "Script" fileEntryWidget
        , padControl SeedSelector "Seed" seedEntryWidget
        ]
