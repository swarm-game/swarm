{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.View.ViewUtils where

import Brick hiding (Direction)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border (hBorder, hBorderWithLabel, joinableBorder, vBorder)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (getEditContents, renderEditor)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.Table qualified as BT
import Control.Lens hiding (Const, from)
import Control.Monad (guard)
import Control.Monad.Reader (withReaderT)
import Data.Array (range)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable qualified as F
import Data.Functor (($>))
import Data.IntMap qualified as IM
import Data.List (intersperse)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup (sconcat)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Graphics.Vty qualified as V
import Linear
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.Scenario (scenarioAuthor, scenarioDescription, scenarioName, scenarioObjectives)
import Swarm.Game.ScenarioInfo (
  ScenarioItem (..),
  ScenarioStatus (..),
  scenarioBestTicks,
  scenarioBestTime,
  scenarioItemName,
  scenarioStatus,
 )
import Swarm.Game.State
import Swarm.Game.Terrain (TerrainType, terrainMap)
import Swarm.Game.World qualified as W
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types (Polytype)
import Swarm.TUI.Attr
import Swarm.TUI.Border
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Inventory.Sorting (renderSortMethod)
import Swarm.TUI.Model
import Swarm.TUI.Panel
import Swarm.Util
import Swarm.Version (NewReleaseFailure (..))
import System.Clock (TimeSpec (..))
import Text.Printf
import Text.Wrap
import Witch (from, into)

-- | Generate a fresh modal window of the requested type.
generateModal :: AppState -> ModalType -> Modal
generateModal s mt = Modal mt (dialog (Just title) buttons (maxModalWindowWidth `min` requiredWidth))
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
                ( 0
                , [ (nextMsg, NextButton scene)
                  | Just scene <- [nextScenario (s ^. uiState . uiMenu)]
                  ]
                    ++ [ (stopMsg, QuitButton)
                       , (continueMsg, KeepPlayingButton)
                       ]
                )
            , sum (map length [nextMsg, stopMsg, continueMsg]) + 32
            )
      DescriptionModal e -> (descriptionTitle e, Nothing, descriptionWidth)
      QuitModal ->
        let stopMsg = fromMaybe ("Quit to" ++ maybe "" (" " ++) (into @String <$> curMenuName s) ++ " menu") haltingMessage
            maybeStartOver = sequenceA ("Start over", StartOverButton currentSeed <$> currentScenario)
         in ( ""
            , Just
                ( 0
                , catMaybes
                    [ Just ("Keep playing", CancelButton)
                    , maybeStartOver
                    , Just (stopMsg, QuitButton)
                    ]
                )
            , T.length (quitMsg (s ^. uiState . uiMenu)) + 4
            )
      GoalModal _ ->
        let goalModalTitle = case currentScenario of
              Nothing -> "Goal"
              Just (scenario, _) -> scenario ^. scenarioName
         in (" " <> T.unpack goalModalTitle <> " ", Nothing, 80)
      KeepPlayingModal -> ("", Just (0, [("OK", CancelButton)]), 80)
      TerrainPaletteModal -> ("Terrain", Nothing, w)
       where
        wordLength = maximum $ map (length . show) (listEnums :: [TerrainType])
        w = wordLength + 6



-- | Render the type of the current REPL input to be shown to the user.
drawType :: Polytype -> Widget Name
drawType = withAttr infoAttr . padLeftRight 1 . txt . prettyText



drawLabeledTerrainSwatch :: TerrainType -> Widget Name
drawLabeledTerrainSwatch a =
  tile <+> str materialName
 where
  tile = padRight (Pad 1) $ renderDisplay $ terrainMap M.! a
  materialName = init $ show a


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