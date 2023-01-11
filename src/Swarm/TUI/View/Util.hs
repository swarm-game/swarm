{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.View.Util where

import Brick hiding (Direction)
import Brick.Widgets.Dialog
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Control.Monad.Reader (withReaderT)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Swarm.Game.Entity as E
import Swarm.Game.Scenario (scenarioName)
import Swarm.Game.ScenarioInfo (scenarioItemName)
import Swarm.Game.State
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Types (Polytype)
import Swarm.TUI.Attr
import Swarm.TUI.Model
import Swarm.TUI.Model.UI
import Witch (from, into)

-- | Generate a fresh modal window of the requested type.
generateModal :: AppState -> ModalType -> Modal
generateModal s mt = Modal mt (dialog (Just $ str title) buttons (maxModalWindowWidth `min` requiredWidth))
 where
  currentScenario = s ^. uiState . scenarioRef
  currentSeed = s ^. gameState . seed
  haltingMessage = case s ^. uiState . uiMenu of
    NoMenu -> Just "Quit"
    _ -> Nothing
  descriptionWidth = 100
  helpWidth = 80
  (title, buttons, requiredWidth) =
    case mt of
      HelpModal -> (" Help ", Nothing, helpWidth)
      RobotsModal -> ("Robots", Nothing, descriptionWidth)
      RecipesModal -> ("Available Recipes", Nothing, descriptionWidth)
      CommandsModal -> ("Available Commands", Nothing, descriptionWidth)
      MessagesModal -> ("Messages", Nothing, descriptionWidth)
      WinModal ->
        let nextMsg = "Next challenge!"
            stopMsg = fromMaybe "Return to the menu" haltingMessage
            continueMsg = "Keep playing"
         in ( ""
            , Just
                ( GenericModalName
                , [ (nextMsg, GenericModalName, NextButton scene)
                  | Just scene <- [nextScenario (s ^. uiState . uiMenu)]
                  ]
                    ++ [ (stopMsg, GenericModalName, QuitButton)
                       , (continueMsg, GenericModalName, KeepPlayingButton)
                       ]
                )
            , sum (map length [nextMsg, stopMsg, continueMsg]) + 32
            )
      LoseModal ->
        let stopMsg = fromMaybe "Return to the menu" haltingMessage
            continueMsg = "Keep playing"
            maybeStartOver = do
              cs <- currentScenario
              return ("Start over", GenericModalName, StartOverButton currentSeed cs)
         in ( ""
            , Just
                ( GenericModalName
                , catMaybes
                    [ Just (stopMsg, GenericModalName, QuitButton)
                    , maybeStartOver
                    , Just (continueMsg, GenericModalName, KeepPlayingButton)
                    ]
                )
            , sum (map length [stopMsg, continueMsg]) + 32
            )
      DescriptionModal e -> (descriptionTitle e, Nothing, descriptionWidth)
      QuitModal ->
        let stopMsg = fromMaybe ("Quit to" ++ maybe "" (" " ++) (into @String <$> curMenuName s) ++ " menu") haltingMessage
            maybeStartOver = do
              cs <- currentScenario
              return ("Start over", GenericModalName, StartOverButton currentSeed cs)
         in ( ""
            , Just
                ( GenericModalName
                , catMaybes
                    [ Just ("Keep playing", GenericModalName, CancelButton)
                    , maybeStartOver
                    , Just (stopMsg, GenericModalName, QuitButton)
                    ]
                )
            , T.length (quitMsg (s ^. uiState . uiMenu)) + 4
            )
      GoalModal ->
        let goalModalTitle = case currentScenario of
              Nothing -> "Goal"
              Just (scenario, _) -> scenario ^. scenarioName
         in (" " <> T.unpack goalModalTitle <> " ", Nothing, descriptionWidth)
      KeepPlayingModal -> ("", Just (GenericModalName, [("OK", GenericModalName, CancelButton)]), 80)

-- | Render the type of the current REPL input to be shown to the user.
drawType :: Polytype -> Widget Name
drawType = withAttr infoAttr . padLeftRight 1 . txt . prettyText

descriptionTitle :: Entity -> String
descriptionTitle e = " " ++ from @Text (e ^. entityName) ++ " "

-- | Width cap for modal and error message windows
maxModalWindowWidth :: Int
maxModalWindowWidth = 500

-- | Get the name of the current New Game menu.
curMenuName :: AppState -> Maybe Text
curMenuName s = case s ^. uiState . uiMenu of
  NewGameMenu (_ :| (parentMenu : _)) ->
    Just (parentMenu ^. BL.listSelectedElementL . to scenarioItemName)
  NewGameMenu _ -> Just "Scenarios"
  _ -> Nothing

quitMsg :: Menu -> Text
quitMsg m = "Are you sure you want to " <> quitAction <> "? All progress on this scenario will be lost!"
 where
  quitAction = case m of
    NoMenu -> "quit"
    _ -> "return to the menu"

-- | Display a list of text-wrapped paragraphs with one blank line after
--   each.
displayParagraphs :: [Text] -> Widget Name
displayParagraphs = vBox . map (padBottom (Pad 1) . txtWrap)

withEllipsis :: Text -> Widget Name
withEllipsis t =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        ellipsis = T.replicate 3 $ T.singleton '.'
        tLength = T.length t
        newText =
          if tLength > w
            then T.take (w - T.length ellipsis) t <> ellipsis
            else t
    render $ txt newText

-- | Make a widget scrolling if it is bigger than the available
--   vertical space.  Thanks to jtdaugherty for this code.
maybeScroll :: (Ord n, Show n) => n -> Widget n -> Widget n
maybeScroll vpName contents =
  Widget Greedy Greedy $ do
    ctx <- getContext
    result <- withReaderT (availHeightL .~ 10000) (render contents)
    if V.imageHeight (result ^. imageL) <= ctx ^. availHeightL
      then return result
      else
        render $
          withVScrollBars OnRight $
            viewport vpName Vertical $
              Widget Fixed Fixed $
                return result
