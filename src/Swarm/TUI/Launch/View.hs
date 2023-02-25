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
import Control.Lens
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Scenario (scenarioSeed)
import Swarm.TUI.Attr
import Swarm.TUI.Launch.Model
import Swarm.TUI.Model.Name

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
      (hCenter . txt)
      [ "Up/Down: select"
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
        ]
          <> footerRows

optionDescription :: ScenarioConfigPanelFocusable -> Maybe Text
optionDescription = \case
  SeedSelector -> Just "Leaving this field blank will use the default seed for the scenario."
  ScriptSelector -> Just "Selecting a script to be run upon start permits eligibility for code size scoring."
  StartGameButton -> Nothing

drawLaunchConfigPanel :: LaunchOptions -> [Widget Name]
drawLaunchConfigPanel (LaunchOptions (LaunchControls (FileBrowserControl fb isFbDisplayed) seedEditor ring displayedFor) _validatedOptions) =
  addFileBrowser [panelWidget]
 where
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

  mkButton name label = highlightIfFocused name $ withAttr boldAttr $ str label

  seedEntryContent = mconcat $ getEditContents seedEditor
  scenarioSeedText = maybe "random" show $ view scenarioSeed . fst =<< displayedFor
  seedEntryWidget =
    if T.null seedEntryContent && not (isFocused SeedSelector)
      then str $ unwords ["scenario default", "(" <> scenarioSeedText <> ")"]
      else
        hLimit 10 $
          overrideAttr E.editFocusedAttr customEditFocusedAttr $
            renderEditor (txt . mconcat) (isFocused SeedSelector) seedEditor

  unspecifiedFileMessage =
    if isFocused ScriptSelector
      then withAttr highlightAttr $ str "<[Enter] to select>"
      else str "<none>"
  fileEntryWidget =
    maybe unspecifiedFileMessage (str . FB.fileInfoSanitizedFilename) $
      listToMaybe $
        FB.fileBrowserSelection fb

  panelWidget =
    centerLayer $
      borderWithLabel (str " Configure scenario launch ") $
        hLimit 60 $
          padAll 1 $
            vBox
              [ controlsBox
              , descriptionBox
              , hCenter $ mkButton StartGameButton ">> Launch with these settings <<"
              ]
   where
    descriptionBox = vLimit 4 $
      padBottom Max $
        padRight (Pad 2) $
          case optionDescription =<< getFocusedConfigPanel of
            Just desc ->
              withDefAttr dimAttr $
                hBox
                  [ padLeft (Pad 6) $ withAttr boldAttr $ str "[Info]"
                  , padLeft (Pad 1) $ txtWrap desc
                  ]
            Nothing -> str " "

    controlsBox =
      vBox
        [ padBottom (Pad 1) $
            padLeft (Pad 2) $
              hBox
                [ mkButton SeedSelector "Seed: "
                , seedEntryWidget
                ]
        , padBottom (Pad 1) $
            padLeft (Pad 2) $
              hBox
                [ mkButton ScriptSelector "Script: "
                , fileEntryWidget
                ]
        ]
