{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Doc.Gen (
  generateDocs,
  GenerateDocs (..),
  EditorType (..),
  SheetType (..),

  -- ** Formatted keyword lists
  keywordsCommands,
  keywordsDirections,
  operatorNames,
  builtinFunctionList,
  editorList,

  -- ** Wiki pages
  PageAddress (..),
  commandsPage,
  capabilityPage,
  noPageAddresses,
) where

import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens (view, (^.))
import Control.Lens.Combinators (to)
import Control.Monad (zipWithM, zipWithM_)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (find, toList)
import Data.List (transpose)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple (swap)
import Swarm.Doc.Pedagogy
import Swarm.Game.Display (displayChar)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityDisplay, entityName, loadEntities)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.Recipe (Recipe, loadRecipes, recipeInputs, recipeOutputs, recipeRequirements, recipeTime, recipeWeight)
import Swarm.Game.Robot (Robot, equippedDevices, instantiateRobot, robotInventory)
import Swarm.Game.Scenario (Scenario, loadScenario, scenarioRobots)
import Swarm.Game.WorldGen (testWorld2Entites)
import Swarm.Language.Capability (Capability)
import Swarm.Language.Capability qualified as Capability
import Swarm.Language.Key (specialKeyNames)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax (Const (..))
import Swarm.Language.Syntax qualified as Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Util (both, listEnums, quote)
import Swarm.Util.Effect (simpleErrorHandle)
import Text.Dot (Dot, NodeId, (.->.))
import Text.Dot qualified as Dot
import Witch (from)
import Swarm.Language.Text.Markdown as Markdown (toText)

-- ============================================================================
-- MAIN ENTRYPOINT TO CLI DOCUMENTATION GENERATOR
-- ============================================================================
--
-- These are the exported functions used by the executable.
--
-- ----------------------------------------------------------------------------

data GenerateDocs where
  -- | Entity dependencies by recipes.
  RecipeGraph :: GenerateDocs
  -- | Keyword lists for editors.
  EditorKeywords :: Maybe EditorType -> GenerateDocs
  -- | List of special key names recognized by 'key' command
  SpecialKeyNames :: GenerateDocs
  CheatSheet :: PageAddress -> Maybe SheetType -> GenerateDocs
  -- | List command introductions by tutorial
  TutorialCoverage :: GenerateDocs
  deriving (Eq, Show)

data EditorType = Emacs | VSCode
  deriving (Eq, Show, Enum, Bounded)

data SheetType = Entities | Commands | Capabilities | Recipes
  deriving (Eq, Show, Enum, Bounded)

data PageAddress = PageAddress
  { entityAddress :: Text
  , commandsAddress :: Text
  , capabilityAddress :: Text
  , recipesAddress :: Text
  }
  deriving (Eq, Show)

noPageAddresses :: PageAddress
noPageAddresses = PageAddress "" "" "" ""

generateDocs :: GenerateDocs -> IO ()
generateDocs = \case
  RecipeGraph -> generateRecipe >>= putStrLn
  EditorKeywords e ->
    case e of
      Just et -> generateEditorKeywords et
      Nothing -> do
        putStrLn "All editor completions:"
        let editorGen et = do
              putStrLn $ replicate 40 '-'
              putStrLn $ "-- " <> show et
              putStrLn $ replicate 40 '-'
              generateEditorKeywords et
        mapM_ editorGen listEnums
  SpecialKeyNames -> generateSpecialKeyNames
  CheatSheet address s -> case s of
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
  TutorialCoverage -> renderTutorialProgression >>= putStrLn . T.unpack

-- ----------------------------------------------------------------------------
-- GENERATE KEYWORDS: LIST OF WORDS TO BE HIGHLIGHTED
-- ----------------------------------------------------------------------------

generateEditorKeywords :: EditorType -> IO ()
generateEditorKeywords = \case
  Emacs -> do
    putStrLn "(x-builtins '("
    T.putStr $ builtinFunctionList Emacs
    putStrLn "))\n(x-commands '("
    T.putStr $ keywordsCommands Emacs
    T.putStr $ keywordsDirections Emacs
    putStrLn "))"
  VSCode -> do
    putStrLn "Functions and commands:"
    T.putStrLn $ builtinFunctionList VSCode <> "|" <> keywordsCommands VSCode
    putStrLn "\nDirections:"
    T.putStrLn $ keywordsDirections VSCode
    putStrLn "\nOperators:"
    T.putStrLn operatorNames

commands :: [Const]
commands = filter Syntax.isCmd Syntax.allConst

operators :: [Const]
operators = filter Syntax.isOperator Syntax.allConst

builtinFunctions :: [Const]
builtinFunctions = filter Syntax.isBuiltinFunction Syntax.allConst

builtinFunctionList :: EditorType -> Text
builtinFunctionList e = editorList e $ map constSyntax builtinFunctions

editorList :: EditorType -> [Text] -> Text
editorList = \case
  Emacs -> T.unlines . map (("  " <>) . quote)
  VSCode -> T.intercalate "|"

constSyntax :: Const -> Text
constSyntax = Syntax.syntax . Syntax.constInfo

-- | Get formatted list of basic functions/commands.
keywordsCommands :: EditorType -> Text
keywordsCommands e = editorList e $ map constSyntax commands

-- | Get formatted list of directions.
keywordsDirections :: EditorType -> Text
keywordsDirections e = editorList e $ map Syntax.directionSyntax Syntax.allDirs

operatorNames :: Text
operatorNames = T.intercalate "|" $ map (escape . constSyntax) operators
 where
  special :: String
  special = "*+$[]|^"
  slashNotComment = \case
    '/' -> "/(?![/|*])"
    c -> T.singleton c
  escape = T.concatMap (\c -> if c `elem` special then T.snoc "\\\\" c else slashNotComment c)

-- ----------------------------------------------------------------------------
-- GENERATE SPECIAL KEY NAMES
-- ----------------------------------------------------------------------------

generateSpecialKeyNames :: IO ()
generateSpecialKeyNames =
  T.putStr . T.unlines . Set.toList $ specialKeyNames

-- ----------------------------------------------------------------------------
-- GENERATE TABLES: COMMANDS, ENTITIES AND CAPABILITIES TO MARKDOWN TABLE
-- ----------------------------------------------------------------------------

wrap :: Char -> Text -> Text
wrap c = T.cons c . flip T.snoc c

codeQuote :: Text -> Text
codeQuote = wrap '`'

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

addLink :: Text -> Text -> Text
addLink l t = T.concat ["[", t, "](", l, ")"]

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- ---------
-- COMMANDS
-- ---------

commandHeader :: [Text]
commandHeader = ["Syntax", "Type", "Capability", "Description"]

commandToList :: Const -> [Text]
commandToList c =
  map
    escapeTable
    [ addLink ("#" <> tshow c) . codeQuote $ constSyntax c
    , codeQuote . prettyText $ inferConst c
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

-- -------------
-- CAPABILITIES
-- -------------

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

-- ---------
-- Entities
-- ---------

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
      <> [Markdown.toText $ view E.entityDescription e]
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

-- -------------
-- RECIPES
-- -------------

recipeHeader :: [Text]
recipeHeader = ["In", "Out", "Required", "Time", "Weight"]

recipeRow :: PageAddress -> Recipe Entity -> [Text]
recipeRow PageAddress {..} r =
  map
    escapeTable
    [ T.intercalate ", " (map formatCE $ view recipeInputs r)
    , T.intercalate ", " (map formatCE $ view recipeOutputs r)
    , T.intercalate ", " (map formatCE $ view recipeRequirements r)
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

-- ----------------------------------------------------------------------------
-- GENERATE GRAPHVIZ: ENTITY DEPENDENCIES BY RECIPES
-- ----------------------------------------------------------------------------

generateRecipe :: IO String
generateRecipe = simpleErrorHandle $ do
  entities <- loadEntities
  recipes <- loadRecipes entities
  classic <- classicScenario
  return . Dot.showDot $ recipesToDot classic entities recipes

recipesToDot :: Scenario -> EntityMap -> [Recipe Entity] -> Dot ()
recipesToDot classic emap recipes = do
  Dot.attribute ("rankdir", "LR")
  Dot.attribute ("ranksep", "2")
  world <- diamond "World"
  base <- diamond "Base"
  -- --------------------------------------------------------------------------
  -- add nodes with for all the known entities
  let enames' = toList . Map.keysSet . entitiesByName $ emap
      enames = filter (`Set.notMember` ignoredEntities) enames'
  ebmap <- Map.fromList . zip enames <$> mapM (box . unpack) enames
  -- --------------------------------------------------------------------------
  -- getters for the NodeId based on entity name or the whole entity
  let safeGetEntity m e = fromMaybe (error $ unpack e <> " is not an entity!?") $ m Map.!? e
      getE = safeGetEntity ebmap
      nid = getE . view entityName
  -- --------------------------------------------------------------------------
  -- Get the starting inventories, entities present in the world and compute
  -- how hard each entity is to get - see 'recipeLevels'.
  let devs = startingDevices classic
      inv = startingInventory classic
      worldEntites = Set.map (safeGetEntity $ entitiesByName emap) testWorld2Entites
      levels = recipeLevels recipes (Set.unions [worldEntites, devs])
  -- --------------------------------------------------------------------------
  -- Base inventory
  (_bc, ()) <- Dot.cluster $ do
    Dot.attribute ("style", "filled")
    Dot.attribute ("color", "lightgrey")
    mapM_ ((base ---<>) . nid) devs
    mapM_ ((base .->.) . nid . fst) $ Map.toList inv
  -- --------------------------------------------------------------------------
  -- World entities
  (_wc, ()) <- Dot.cluster $ do
    Dot.attribute ("style", "filled")
    Dot.attribute ("color", "forestgreen")
    mapM_ ((uncurry (Dot..->.) . (world,)) . getE) (toList testWorld2Entites)
  -- --------------------------------------------------------------------------
  let -- put a hidden node above and below entities and connect them by hidden edges
      wrapBelowAbove :: Set Entity -> Dot (NodeId, NodeId)
      wrapBelowAbove ns = do
        b <- hiddenNode
        t <- hiddenNode
        let ns' = map nid $ toList ns
        mapM_ (b .~>.) ns'
        mapM_ (.~>. t) ns'
        return (b, t)
      -- put set of entities in nice
      subLevel :: Int -> Set Entity -> Dot (NodeId, NodeId)
      subLevel i ns = fmap snd . Dot.cluster $ do
        Dot.attribute ("style", "filled")
        Dot.attribute ("color", "khaki")
        bt <- wrapBelowAbove ns
        Dot.attribute ("rank", "sink")
        -- the normal label for cluster would be cover by lines
        _bigLabel <-
          Dot.node
            [ ("shape", "plain")
            , ("label", "Bottom Label")
            , ("fontsize", "20pt")
            , ("label", "Level #" <> show i)
            ]
        return bt
  -- --------------------------------------------------------------------------
  -- order entities into clusters based on how "far" they are from
  -- what is available at the start - see 'recipeLevels'.
  bottom <- wrapBelowAbove worldEntites
  ls <- zipWithM subLevel [1 ..] (tail levels)
  let invisibleLine = zipWithM_ (.~>.)
  tls <- mapM (const hiddenNode) levels
  bls <- mapM (const hiddenNode) levels
  invisibleLine tls bls
  invisibleLine bls (tail tls)
  let sameBelowAbove (b1, t1) (b2, t2) = Dot.same [b1, b2] >> Dot.same [t1, t2]
  zipWithM_ sameBelowAbove (bottom : ls) (zip bls tls)
  -- --------------------------------------------------------------------------
  -- add node for the world and draw a line to each entity found in the wild
  -- finally draw recipes
  let recipeInOut r = [(snd i, snd o) | i <- r ^. recipeInputs, o <- r ^. recipeOutputs]
      recipeReqOut r = [(snd q, snd o) | q <- r ^. recipeRequirements, o <- r ^. recipeOutputs]
      recipesToPairs f rs = both nid <$> nubOrd (concatMap f rs)
  mapM_ (uncurry (.->.)) (recipesToPairs recipeInOut recipes)
  mapM_ (uncurry (---<>)) (recipesToPairs recipeReqOut recipes)

-- ----------------------------------------------------------------------------
-- RECIPE LEVELS
-- ----------------------------------------------------------------------------

-- | Order entities in sets depending on how soon it is possible to obtain them.
--
-- So:
--  * Level 0 - starting entities (for example those obtainable in the world)
--  * Level N+1 - everything possible to make (or drill) from Level N
--
-- This is almost a BFS, but the requirement is that the set of entities
-- required for recipe is subset of the entities known in Level N.
--
-- If we ever depend on some graph library, this could be rewritten
-- as some BFS-like algorithm with added recipe nodes, but you would
-- need to enforce the condition that recipes need ALL incoming edges.
recipeLevels :: [Recipe Entity] -> Set Entity -> [Set Entity]
recipeLevels recipes start = levels
 where
  recipeParts r = ((r ^. recipeInputs) <> (r ^. recipeRequirements), r ^. recipeOutputs)
  m :: [(Set Entity, Set Entity)]
  m = map (both (Set.fromList . map snd) . recipeParts) recipes
  levels :: [Set Entity]
  levels = reverse $ go [start] start
   where
    isKnown known (i, _o) = null $ i Set.\\ known
    nextLevel known = Set.unions . map snd $ filter (isKnown known) m
    go ls known =
      let n = nextLevel known Set.\\ known
       in if null n
            then ls
            else go (n : ls) (Set.union n known)

-- | Get classic scenario to figure out starting entities.
classicScenario :: (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) => m Scenario
classicScenario = do
  entities <- loadEntities
  fst <$> loadScenario "data/scenarios/classic.yaml" entities

startingHelper :: Scenario -> Robot
startingHelper = instantiateRobot 0 . head . view scenarioRobots

startingDevices :: Scenario -> Set Entity
startingDevices = Set.fromList . map snd . E.elems . view equippedDevices . startingHelper

startingInventory :: Scenario -> Map Entity Int
startingInventory = Map.fromList . map swap . E.elems . view robotInventory . startingHelper

-- | Ignore utility entities that are just used for tutorials and challenges.
ignoredEntities :: Set Text
ignoredEntities =
  Set.fromList
    [ "upper left corner"
    , "upper right corner"
    , "lower left corner"
    , "lower right corner"
    , "horizontal wall"
    , "vertical wall"
    ]

-- ----------------------------------------------------------------------------
-- GRAPHVIZ HELPERS
-- ----------------------------------------------------------------------------

customNode :: [(String, String)] -> String -> Dot NodeId
customNode attrs label = Dot.node $ [("style", "filled"), ("label", label)] <> attrs

box, diamond :: String -> Dot NodeId
box = customNode [("shape", "box")]
diamond = customNode [("shape", "diamond")]

-- | Hidden node - used for layout.
hiddenNode :: Dot NodeId
hiddenNode = Dot.node [("style", "invis")]

-- | Hidden edge - used for layout.
(.~>.) :: NodeId -> NodeId -> Dot ()
i .~>. j = Dot.edge i j [("style", "invis")]

-- | Edge for recipe requirements and outputs.
(---<>) :: NodeId -> NodeId -> Dot ()
e1 ---<> e2 = Dot.edge e1 e2 attrs
 where
  attrs = [("arrowhead", "diamond"), ("color", "blue")]
