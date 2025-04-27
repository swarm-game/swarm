{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Util where

import Brick hiding (Direction, Location)
import Brick.Keybindings (Binding (..), KeyConfig, firstActiveBinding, ppBinding)
import Brick.Widgets.Dialog
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Control.Monad.Reader (withReaderT)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Swarm.Game.Entity as E
import Swarm.Game.Land
import Swarm.Game.Scenario (scenarioMetadata, scenarioName)
import Swarm.Game.Scenario.Status
import Swarm.Game.ScenarioInfo (scenarioItemName)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Substate
import Swarm.Game.Terrain
import Swarm.Language.Syntax (Syntax)
import Swarm.Language.Text.Markdown qualified as Markdown
import Swarm.Language.Types (Polytype)
import Swarm.Pretty (prettyTextLine)
import Swarm.TUI.Model
import Swarm.TUI.Model.Event (SwarmEvent)
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.Util (maximum0)
import Witch (from, into)

generateScenarioEndModal :: Menu -> PlayState -> EndScenarioModalType -> Modal
generateScenarioEndModal m s mt =
  Modal (EndScenarioModal mt) (dialog (Just $ str title) buttons (maxModalWindowWidth `min` requiredWidth))
 where
  currentScenario = s ^. scenarioState . uiGameplay . scenarioRef
  currentSeed = s ^. scenarioState . gameState . randomness . seed

  scenarioList = s ^. progression . scenarioSequence

  (title, buttons, requiredWidth) = case mt of
    ScenarioFinishModal WinModal -> mkWinModal
    ScenarioFinishModal LoseModal -> mkLoseModal
    QuitModal -> mkQuitModal
    KeepPlayingModal -> ("", Just (Button CancelButton, [("OK", Button CancelButton, Cancel)]), 80)

  mkWinModal =
    ( ""
    , Just
        ( Button NextButton
        , [ (nextMsg, Button NextButton, Next remainingScenarios)
          | Just remainingScenarios <- [NE.nonEmpty scenarioList]
          ]
            ++ [ (stopMsg, Button QuitButton, QuitAction) -- TODO(#2376) QuitAction is not used
               , (continueMsg, Button KeepPlayingButton, KeepPlaying)
               ]
        )
    , sum (map length [nextMsg, stopMsg, continueMsg]) + 32
    )
   where
    nextMsg = "Next challenge!"

  maybeStartOver = do
    cs <- currentScenario
    return ("Start over", Button StartOverButton, StartOver currentSeed cs)

  mkLoseModal =
    ( ""
    , Just
        ( Button $ if isJust currentScenario then StartOverButton else QuitButton
        , catMaybes
            [ Just (stopMsg, Button QuitButton, QuitAction)
            , maybeStartOver
            , Just (continueMsg, Button KeepPlayingButton, KeepPlaying)
            ]
        )
    , sum (map length [stopMsg, continueMsg]) + 32
    )

  haltingMessage =
    if isNoMenu
      then Just "Quit"
      else Nothing

  stopMsg = fromMaybe ("Quit to" ++ maybe "" (" " ++) (into @String <$> curMenuName m) ++ " menu") haltingMessage
  continueMsg = "Keep playing"

  isNoMenu = case m of
    NoMenu -> True
    _ -> False

  mkQuitModal =
    ( ""
    , Just
        ( Button CancelButton
        , catMaybes
            [ Just ("Keep playing", Button CancelButton, Cancel)
            , maybeStartOver
            , Just (stopMsg, Button QuitButton, QuitAction)
            ]
        )
    , T.length (quitMsg isNoMenu) + 4
    )

-- | Generate a fresh modal window of the requested type.
generateModal :: ScenarioState -> MidScenarioModalType -> Modal
generateModal s mt =
  Modal (MidScenarioModal mt) (dialog (Just $ str title) buttons (maxModalWindowWidth `min` requiredWidth))
 where
  currentScenario = s ^. uiGameplay . scenarioRef

  descriptionWidth = 100
  (title, buttons, requiredWidth) =
    case mt of
      HelpModal -> (" Help ", Nothing, descriptionWidth)
      RobotsModal -> ("Robots", Nothing, descriptionWidth)
      RecipesModal -> ("Available Recipes", Nothing, descriptionWidth)
      CommandsModal -> ("Available Commands", Nothing, descriptionWidth)
      MessagesModal -> ("Messages", Nothing, descriptionWidth)
      StructuresModal -> ("Buildable Structures", Nothing, descriptionWidth)
      DescriptionModal e -> (descriptionTitle e, Nothing, descriptionWidth)
      GoalModal ->
        let goalModalTitle = case currentScenario of
              Nothing -> "Goal"
              Just (ScenarioWith scenario _) -> scenario ^. scenarioMetadata . scenarioName
         in (" " <> T.unpack goalModalTitle <> " ", Nothing, descriptionWidth)
      TerrainPaletteModal -> ("Terrain", Nothing, w)
       where
        tm = s ^. gameState . landscape . terrainAndEntities . terrainMap
        wordLength = maximum0 $ map (T.length . getTerrainWord) (M.keys $ terrainByName tm)
        w = wordLength + 6
      EntityPaletteModal -> ("Entity", Nothing, 30)

-- | Render the type of the current REPL input to be shown to the user.
drawType :: Polytype -> Widget Name
drawType ty = Widget Fixed Fixed $ do
  ctx <- getContext
  let w = ctx ^. availWidthL
      renderedTy = prettyTextLine ty
      displayedTy
        | T.length renderedTy <= w `div` 2 - 2 = renderedTy
        | otherwise = T.take (w `div` 2 - 2 - 3) renderedTy <> "..."
  render . withAttr infoAttr . padLeftRight 1 . txt $ displayedTy

-- | Draw markdown document with simple code/bold/italic attributes.
--
-- TODO: #574 Code blocks should probably be handled separately.
drawMarkdown :: Markdown.Document Syntax -> Widget Name
drawMarkdown d = do
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
    let docLines = Markdown.chunksOf w . Markdown.toStream <$> Markdown.paragraphs d
    render . layoutParagraphs $ vBox . map (hBox . map mTxt) <$> docLines
 where
  mTxt = \case
    Markdown.TextNode as t -> foldr applyAttr (txt t) as
    Markdown.CodeNode t -> withAttr highlightAttr $ txt t
    Markdown.RawNode f t -> withAttr (rawAttr f) $ txt t
  applyAttr a = withAttr $ case a of
    Markdown.Strong -> boldAttr
    Markdown.Emphasis -> italicAttr
  rawAttr = \case
    "entity" -> greenAttr
    "structure" -> redAttr
    "tag" -> yellowAttr
    "robot" -> beigeAttr
    "type" -> magentaAttr
    _snippet -> highlightAttr -- same as plain code

drawLabeledTerrainSwatch :: TerrainMap -> TerrainType -> Widget Name
drawLabeledTerrainSwatch tm a =
  tile <+> str materialName
 where
  tile =
    padRight (Pad 1)
      . renderDisplay
      . maybe mempty terrainDisplay
      $ M.lookup a (terrainByName tm)

  materialName = init $ show a

descriptionTitle :: Entity -> String
descriptionTitle e = " " ++ from @Text (e ^. entityName) ++ " "

-- | Width cap for modal and error message windows
maxModalWindowWidth :: Int
maxModalWindowWidth = 500

-- | Get the name of the current New Game menu.
curMenuName :: Menu -> Maybe Text
curMenuName m = case m of
  NewGameMenu (_ :| (parentMenu : _)) ->
    Just (parentMenu ^. BL.listSelectedElementL . to scenarioItemName)
  NewGameMenu _ -> Just "Scenarios"
  _ -> Nothing

quitMsg :: Bool -> Text
quitMsg isNoMenu = "Are you sure you want to " <> quitAction <> "? All progress on this scenario will be lost!"
 where
  quitAction =
    if isNoMenu
      then "quit"
      else "return to the menu"

-- | Display a list of text-wrapped paragraphs with one blank line after each.
displayParagraphs :: [Text] -> Widget Name
displayParagraphs = layoutParagraphs . map txtWrap

-- | Display a list of paragraphs with one blank line after each.
--
-- For the common case of `[Text]` use 'displayParagraphs'.
layoutParagraphs :: [Widget Name] -> Widget Name
layoutParagraphs ps = vBox $ padBottom (Pad 1) <$> ps

data EllipsisSide = Beginning | End

withEllipsis :: EllipsisSide -> Text -> Widget Name
withEllipsis side t =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        ellipsis = T.replicate 3 $ T.singleton '.'
        tLength = T.length t
        newText =
          if tLength > w
            then case side of
              Beginning -> ellipsis <> T.drop (w - T.length ellipsis) t
              End -> T.take (w - T.length ellipsis) t <> ellipsis
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
        render
          . withVScrollBars OnRight
          . viewport vpName Vertical
          . Widget Fixed Fixed
          $ return result

-- | Draw the name of an entity, labelled with its visual
--   representation as a cell in the world.
drawLabelledEntityName :: Entity -> Widget n
drawLabelledEntityName e =
  hBox
    [ padRight (Pad 2) (renderDisplay (e ^. entityDisplay))
    , txt (e ^. entityName)
    ]

-- | Render the keybinding bound to a specific event.
bindingText :: KeyConfig SwarmEvent -> SwarmEvent -> Text
bindingText keyConf e = maybe "" ppBindingShort b
 where
  b = firstActiveBinding keyConf e
  ppBindingShort = \case
    Binding V.KUp m | null m -> "↑"
    Binding V.KDown m | null m -> "↓"
    Binding V.KLeft m | null m -> "←"
    Binding V.KRight m | null m -> "→"
    bi -> ppBinding bi
