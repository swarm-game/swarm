{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Auto-generation of cheat sheets for the wiki.
module Swarm.Doc.Wiki.Cheatsheet (
  PageAddress (..),
  SheetType (..),
  makeWikiPage,
) where

import Control.Effect.Lift
import Control.Lens (view, (^.))
import Control.Lens.Combinators (to)
import Data.Foldable (find, toList)
import Data.List (transpose)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Swarm.Doc.Schema.Render
import Swarm.Doc.Util
import Swarm.Game.Display (displayChar)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityDisplay, entityName, loadEntities)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Recipe (Recipe, loadRecipes, recipeCatalysts, recipeInputs, recipeOutputs, recipeTime, recipeWeight)
import Swarm.Language.Capability (Capability)
import Swarm.Language.Capability qualified as Capability
import Swarm.Language.Pretty (prettyText, prettyTextLine)
import Swarm.Language.Syntax (Const (..))
import Swarm.Language.Syntax qualified as Syntax
import Swarm.Language.Text.Markdown as Markdown (docToMark)
import Swarm.Language.Typecheck (inferConst)
import Swarm.Util (listEnums)
import Swarm.Util.Effect (simpleErrorHandle)

-- * Types

-- | A configuration record holding the URLs of the various cheat
--   sheets, to facilitate cross-linking.
data PageAddress = PageAddress
  { entityAddress :: Text
  , commandsAddress :: Text
  , capabilityAddress :: Text
  , recipesAddress :: Text
  }
  deriving (Eq, Show)

-- | An enumeration of the kinds of cheat sheets we can produce.
data SheetType = Entities | Commands | Capabilities | Recipes | Scenario
  deriving (Eq, Show, Enum, Bounded)

-- * Functions

makeWikiPage :: PageAddress -> Maybe SheetType -> IO ()
makeWikiPage address s = case s of
  Nothing -> error "Not implemented for all Wikis"
  Just st -> case st of
    Commands -> T.putStrLn commandsPage
    Capabilities -> simpleErrorHandle $ do
      entities <- loadEntities
      sendIO $ T.putStrLn $ capabilityPage address entities
    Entities -> simpleErrorHandle $ do
      entities <- loadEntities
      sendIO $ T.putStrLn $ entitiesPage address (Map.elems $ entitiesByName entities)
    Recipes -> simpleErrorHandle $ do
      entities <- loadEntities
      recipes <- loadRecipes entities
      sendIO $ T.putStrLn $ recipePage address recipes
    Scenario -> genScenarioSchemaDocs

-- ----------------------------------------------------------------------------
-- GENERATE TABLES: COMMANDS, ENTITIES AND CAPABILITIES TO MARKDOWN TABLE
-- ----------------------------------------------------------------------------

escapeTable :: Text -> Text
escapeTable = T.concatMap (\c -> if c == '|' then T.snoc "\\" c else T.singleton c)

separatingLine :: [Int] -> Text
separatingLine ws = T.cons '|' . T.concat $ map (flip T.snoc '|' . flip T.replicate "-" . (2 +)) ws

listToRow :: [Int] -> [Text] -> Text
listToRow mw xs = wrap '|' . T.intercalate "|" $ zipWith format mw xs
 where
  format w x = wrap ' ' x <> T.replicate (w - T.length x) " "

maxWidths :: [[Text]] -> [Int]
maxWidths = map (maximum . map T.length) . transpose

-- ** COMMANDS

commandHeader :: [Text]
commandHeader = ["Syntax", "Type", "Capability", "Description"]

commandToList :: Const -> [Text]
commandToList c =
  map
    escapeTable
    [ addLink ("#" <> tshow c) . codeQuote $ constSyntax c
    , codeQuote . prettyTextLine $ inferConst c
    , maybe "" Capability.capabilityName $ Capability.constCaps c
    , Syntax.briefDoc . Syntax.constDoc $ Syntax.constInfo c
    ]

constTable :: [Const] -> Text
constTable cs = T.unlines $ header <> map (listToRow mw) commandRows
 where
  mw = maxWidths (commandHeader : commandRows)
  commandRows = map commandToList cs
  header = [listToRow mw commandHeader, separatingLine mw]

commandToSection :: Const -> Text
commandToSection c =
  T.unlines $
    [ "## " <> T.pack (show c)
    , ""
    , "- syntax: " <> codeQuote (constSyntax c)
    , "- type: " <> (codeQuote . prettyText $ inferConst c)
    , maybe "" (("- required capabilities: " <>) . Capability.capabilityName) $ Capability.constCaps c
    , ""
    , Syntax.briefDoc . Syntax.constDoc $ Syntax.constInfo c
    ]
      <> let l = Syntax.longDoc . Syntax.constDoc $ Syntax.constInfo c
          in if T.null l then [] else ["", l]

commandsPage :: Text
commandsPage =
  T.intercalate "\n\n" $
    [ "# Commands"
    , constTable commands
    , "# Builtin functions"
    , "These functions are evaluated immediately once they have enough arguments."
    , constTable builtinFunctions
    , "# Operators"
    , constTable operators
    , "# Detailed descriptions"
    ]
      <> map commandToSection (commands <> builtinFunctions <> operators)

-- ** CAPABILITIES

capabilityHeader :: [Text]
capabilityHeader = ["Name", "Commands", "Entities"]

capabilityRow :: PageAddress -> EntityMap -> Capability -> [Text]
capabilityRow PageAddress {..} em cap =
  map
    escapeTable
    [ Capability.capabilityName cap
    , T.intercalate ", " (linkCommand <$> cs)
    , T.intercalate ", " (linkEntity . view entityName <$> es)
    ]
 where
  linkEntity t =
    if T.null entityAddress
      then t
      else addLink (entityAddress <> "#" <> T.replace " " "-" t) t
  linkCommand c =
    ( if T.null commandsAddress
        then id
        else addLink (commandsAddress <> "#" <> tshow c)
    )
      . codeQuote
      $ constSyntax c

  cs = [c | c <- Syntax.allConst, let mcap = Capability.constCaps c, isJust $ find (== cap) mcap]
  es = fromMaybe [] $ E.entitiesByCap em Map.!? cap

capabilityTable :: PageAddress -> EntityMap -> [Capability] -> Text
capabilityTable a em cs = T.unlines $ header <> map (listToRow mw) capabilityRows
 where
  mw = maxWidths (capabilityHeader : capabilityRows)
  capabilityRows = map (capabilityRow a em) cs
  header = [listToRow mw capabilityHeader, separatingLine mw]

capabilityPage :: PageAddress -> EntityMap -> Text
capabilityPage a em = capabilityTable a em listEnums

-- ** Entities

entityHeader :: [Text]
entityHeader = ["?", "Name", "Capabilities", "Properties*", "Portable"]

entityToList :: Entity -> [Text]
entityToList e =
  map
    escapeTable
    [ codeQuote . T.singleton $ e ^. entityDisplay . to displayChar
    , addLink ("#" <> linkID) $ view entityName e
    , T.intercalate ", " $ Capability.capabilityName <$> Set.toList (view E.entityCapabilities e)
    , T.intercalate ", " . map tshow . filter (/= E.Portable) $ toList props
    , if E.Portable `elem` props
        then ":heavy_check_mark:"
        else ":negative_squared_cross_mark:"
    ]
 where
  props = view E.entityProperties e
  linkID = T.replace " " "-" $ view entityName e

entityTable :: [Entity] -> Text
entityTable es = T.unlines $ header <> map (listToRow mw) entityRows
 where
  mw = maxWidths (entityHeader : entityRows)
  entityRows = map entityToList es
  header = [listToRow mw entityHeader, separatingLine mw]

entityToSection :: Entity -> Text
entityToSection e =
  T.unlines $
    [ "## " <> view E.entityName e
    , ""
    , " - Char: " <> (codeQuote . T.singleton $ e ^. entityDisplay . to displayChar)
    ]
      <> [" - Properties: " <> T.intercalate ", " (map tshow $ toList props) | not $ null props]
      <> [" - Capabilities: " <> T.intercalate ", " (Capability.capabilityName <$> caps) | not $ null caps]
      <> ["\n"]
      <> [Markdown.docToMark $ view E.entityDescription e]
 where
  props = view E.entityProperties e
  caps = Set.toList $ view E.entityCapabilities e

entitiesPage :: PageAddress -> [Entity] -> Text
entitiesPage _a es =
  T.intercalate "\n\n" $
    [ "# Entities"
    , "This is a quick-overview table of entities - click the name for detailed description."
    , "*) As a note, most entities have the Portable property, so we show it in a separate column."
    , entityTable es
    ]
      <> map entityToSection es

-- ** RECIPES

recipeHeader :: [Text]
recipeHeader = ["In", "Out", "Required", "Time", "Weight"]

recipeRow :: PageAddress -> Recipe Entity -> [Text]
recipeRow PageAddress {..} r =
  map
    escapeTable
    [ T.intercalate ", " (map formatCE $ view recipeInputs r)
    , T.intercalate ", " (map formatCE $ view recipeOutputs r)
    , T.intercalate ", " (map formatCE $ view recipeCatalysts r)
    , tshow $ view recipeTime r
    , tshow $ view recipeWeight r
    ]
 where
  formatCE (c, e) = T.unwords [tshow c, linkEntity $ view entityName e]
  linkEntity t =
    if T.null entityAddress
      then t
      else addLink (entityAddress <> "#" <> T.replace " " "-" t) t

recipeTable :: PageAddress -> [Recipe Entity] -> Text
recipeTable a rs = T.unlines $ header <> map (listToRow mw) recipeRows
 where
  mw = maxWidths (recipeHeader : recipeRows)
  recipeRows = map (recipeRow a) rs
  header = [listToRow mw recipeHeader, separatingLine mw]

recipePage :: PageAddress -> [Recipe Entity] -> Text
recipePage = recipeTable