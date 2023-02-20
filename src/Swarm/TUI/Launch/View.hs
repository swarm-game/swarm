{-# LANGUAGE OverloadedStrings #-}

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
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Swarm.TUI.Launch.Model
import Swarm.TUI.Attr
import Swarm.TUI.Model.Name

drawFileBrowser :: FB.FileBrowser Name -> Widget Name
drawFileBrowser b =
  centerLayer $ hLimit 50 $ ui <=> help
 where
  ui =
    vLimit 15 $
      borderWithLabel (txt "Choose a file") $
        FB.renderFileBrowser True b

  footerRows = map (hCenter . txt) [
      "Up/Down: select"
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
              hCenter $
                withDefAttr BF.invalidFormInputAttr $
                  txt $
                    T.pack $
                      E.displayException e
        ] <> footerRows

drawLaunchConfigPanel :: LaunchOptions -> [Widget Name]
drawLaunchConfigPanel (LaunchOptions (LaunchControls (FileBrowserControl fb isFbDisplayed) seedEditor ring _isDisplayedFor) _validatedOptions) =
 addFileBrowser [panelWidget]
 where
  addFileBrowser = if isFbDisplayed
    then (drawFileBrowser fb:)
    else id

  isFocused x = focusGetCurrent ring == Just (ScenarioConfigControl (ScenarioConfigPanelControl x))

  highlightIfFocused x =
    if isFocused x
      then withAttr highlightAttr
      else id

  mkButton name label = highlightIfFocused name $ str label

  seedEntryContent = mconcat $ getEditContents seedEditor
  seedEntryWidget = if T.null seedEntryContent && not (isFocused SeedSelector)
    then str "<scenario default>"
    else hLimit 10 $
      overrideAttr E.editFocusedAttr customEditFocusedAttr $
        renderEditor (txt . mconcat) (isFocused SeedSelector) seedEditor

  unspecifiedFileMessage = if isFocused ScriptSelector
    then "<Hit [Enter] to select>"
    else "<none>"
  fileEntryWidget = str $
    maybe unspecifiedFileMessage FB.fileInfoSanitizedFilename $
      listToMaybe $
        FB.fileBrowserSelection fb

  panelWidget =
    centerLayer $
      borderWithLabel (str "Configure scenario launch") $
        hLimit 50 $ padAll 1 $ 
          vBox
            [ padBottom (Pad 1) $ txtWrap "Leaving this field blank will use the default seed for the scenario."
            , padBottom (Pad 1) $ padLeft (Pad 2) $ hBox
                [ mkButton SeedSelector "Seed: "
                , seedEntryWidget
                ]
            , padBottom (Pad 1) $ txtWrap "Selecting a script to be run upon start enables eligibility for code size scoring."
            , padBottom (Pad 1) $ padLeft (Pad 2) $ hBox
                [ mkButton ScriptSelector "Script: "
                , fileEntryWidget
                ]
            , hCenter $ mkButton StartGameButton ">> Launch with these settings <<"
            ]
