{-# LANGUAGE OverloadedStrings #-}

module Swarm.DocGen (
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
  commandsPage,
) where

import Control.Lens (view, (^.), _3)
import Control.Monad (zipWithM, zipWithM_, (<=<))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (transpose)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple (swap)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityName, loadEntities)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Recipe (Recipe, loadRecipes, recipeInputs, recipeOutputs, recipeRequirements)
import Swarm.Game.Robot (installedDevices, instantiateRobot, robotInventory)
import Swarm.Game.Scenario (Scenario, loadScenario, scenarioRobots)
import Swarm.Game.WorldGen (testWorld2Entites)
import Swarm.Language.Capability (capabilityName, constCaps)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax (Const (..))
import Swarm.Language.Syntax qualified as Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Util (isRightOr)
import Text.Dot (Dot, NodeId, (.->.))
import Text.Dot qualified as Dot

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
  CheatSheet :: Maybe SheetType -> GenerateDocs
  deriving (Eq, Show)

data EditorType = Emacs | VSCode
  deriving (Eq, Show, Enum, Bounded)

data SheetType = Entities | Commands | Capabilities | Recipes
  deriving (Eq, Show, Enum, Bounded)

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
        mapM_ editorGen [minBound .. maxBound]
  CheatSheet s -> case s of
    Nothing -> error "Not implemented"
    Just st -> case st of
      Commands -> T.putStrLn commandsPage
      _ -> error "Not implemented"

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
 where
  quote = T.cons '"' . flip T.snoc '"'

constSyntax :: Const -> Text
constSyntax = Syntax.syntax . Syntax.constInfo

-- | Get formatted list of basic functions/commands.
keywordsCommands :: EditorType -> Text
keywordsCommands e = editorList e $ map constSyntax commands

-- | Get formatted list of directions.
keywordsDirections :: EditorType -> Text
keywordsDirections e = editorList e $ map (Syntax.dirSyntax . Syntax.dirInfo) Syntax.allDirs

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

-- ---------
-- COMMANDS
-- ---------

commandHeader :: [Text]
commandHeader = ["Syntax", "Type", "Capability", "Description"]

commandToList :: Const -> [Text]
commandToList c =
  map
    escapeTable
    [ addLink (T.pack $ "#" <> show c) . codeQuote $ constSyntax c
    , codeQuote . prettyText $ inferConst c
    , maybe "" capabilityName $ constCaps c
    , Syntax.briefDoc . Syntax.constDoc $ Syntax.constInfo c
    ]
 where
  addLink l t = T.concat ["[", t, "](", l, ")"]

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
    , maybe "" (("- required capabilities: " <>) . capabilityName) $ constCaps c
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

-- ----------------------------------------------------------------------------
-- GENERATE GRAPHVIZ: ENTITY DEPENDENCIES BY RECIPES
-- ----------------------------------------------------------------------------

generateRecipe :: IO String
generateRecipe = simpleErrorHandle $ do
  entities <- loadEntities >>= guardRight "load entities"
  recipes <- loadRecipes entities >>= guardRight "load recipes"
  classic <- classicScenario
  return . Dot.showDot $ recipesToDot classic entities recipes

recipesToDot :: Scenario -> EntityMap -> [Recipe Entity] -> Dot ()
recipesToDot classic emap recipes = do
  Dot.attribute ("rankdir", "LR")
  Dot.attribute ("ranksep", "2")
  world <- diamond "World"
  base <- diamond "Base"
  -- --------------------------------------------------------------------------
  -- add nodes with for all the known entites
  let enames' = toList . Map.keysSet . entitiesByName $ emap
      enames = filter (`Set.notMember` ignoredEntites) enames'
  ebmap <- Map.fromList . zip enames <$> mapM (box . unpack) enames
  -- --------------------------------------------------------------------------
  -- getters for the NodeId based on entity name or the whole entity
  let safeGetEntity m e = fromMaybe (error $ unpack e <> " is not an entity!?") $ m Map.!? e
      getE = safeGetEntity ebmap
      nid = getE . view entityName
  -- --------------------------------------------------------------------------
  -- Get the starting inventories, entites present in the world and compute
  -- how hard each entity is to get - see 'recipeLevels'.
  let inv = startingInventory classic
      devs = startingDevices classic
      worldEntites = Set.map (safeGetEntity $ entitiesByName emap) testWorld2Entites
      levels = recipeLevels recipes worldEntites devs
  -- --------------------------------------------------------------------------
  -- World entites
  (_wc, ()) <- Dot.cluster $ do
    Dot.attribute ("style", "filled")
    Dot.attribute ("color", "forestgreen")
    mapM_ ((uncurry (Dot..->.) . (world,)) . getE) (toList testWorld2Entites)
  -- --------------------------------------------------------------------------
  let -- put a hidden node above and below entites and connect them by hidden edges
      wrapBelowAbove :: Set Entity -> Dot (NodeId, NodeId)
      wrapBelowAbove ns = do
        b <- hiddenNode
        t <- hiddenNode
        let ns' = map nid $ toList ns
        mapM_ (b .~>.) ns'
        mapM_ (.~>. t) ns'
        return (b, t)
      -- put set of entites in nice
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
  -- order entites into clusters based on how "far" they are from
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
  -- then draw recipes
  let recipeInOut r = [(snd i, snd o) | i <- r ^. recipeInputs, o <- r ^. recipeOutputs]
      recipeReqOut r = [(snd q, snd o) | q <- r ^. recipeRequirements, o <- r ^. recipeOutputs]
      recipesToPairs f rs = both nid <$> nubOrd (concatMap f rs)
  mapM_ (uncurry (.->.)) (recipesToPairs recipeInOut recipes)
  mapM_ (uncurry (---<>)) (recipesToPairs recipeReqOut recipes) -- TODO: command line flag
  -- --------------------------------------------------------------------------
  -- Base inventory
  -- TODO: add a command line flag for this
  let e1 --< e2 = Dot.edge e1 e2 [("style", "dashed")]
  mapM_ ((base --<) . nid . fst) $ Map.toList inv
  -- mark devices you only have installed
  let e1 --<> e2 = Dot.edge e1 e2 [("arrowhead", "diamond"), ("style", "dotted")]
  mapM_ ((base --<>) . nid) (devs Set.\\ Map.keysSet inv)

-- ----------------------------------------------------------------------------
-- RECIPE LEVELS
-- ----------------------------------------------------------------------------

-- | Order entites in sets depending on how soon it is possible to obtain them.
--
-- So:
--  * Level 0 - starting entites (for example those obtainable in the world)
--  * Level N+1 - everything possible to make (or drill) from Level N
--
-- You can think about this as BFS on a graph of sets, where from each set node
-- there is an edge for all recipes that have inputs in the set to the set
-- extended by the recipe outputs.
--
-- Once you factor in required devices you need to think about pairs of sets,
-- where the first part is the set of obtained devices and the second part is
-- the set of devices that can be installed. So the second set is a superset
-- of the first one.
--
-- As a small nitpick, this is not actually true because we did not consider
-- devices required for installing itself! (Also moving, making, building...)
-- But hopefully the game developers are aware of those crucial devices and
-- would not make the game impossible to play. ;) 
--
-- TODO: maybe there is a bug, some entities are not shown in any level
--       but there are recipes that link them
recipeLevels :: [Recipe Entity] -> Set Entity -> Set Entity -> [Set Entity]
recipeLevels recipes start installed = levels
 where
  recipeParts r = (r ^. recipeInputs, r ^. recipeRequirements, r ^. recipeOutputs)
  all3 f (a,b,c) = (f a, f b, f c)
  m :: [(Set Entity, Set Entity, Set Entity)]
  m = map (all3 (Set.fromList . map snd) . recipeParts) recipes
  levels :: [Set Entity]
  levels = go start installed
   where
    go known install =
      let n = nextLevel Set.\\ known
          isKnown (i, r, _o) = null (i Set.\\ known) && null (r Set.\\ install)  
          nextLevel = Set.unions . map (view _3) $ filter isKnown m
       in if null n
            then []
            else n : go (Set.union n known) (Set.union n install)

-- | Get classic scenario to figure out starting entites.
classicScenario :: ExceptT Text IO Scenario
classicScenario = do
  entities <- loadEntities >>= guardRight "load entities"
  fst <$> loadScenario "data/scenarios/classic.yaml" entities

startingDevices :: Scenario -> Set Entity
startingDevices = Set.fromList . map snd . E.elems . view installedDevices . instantiateRobot 0 . head . view scenarioRobots

startingInventory :: Scenario -> Map Entity Int
startingInventory = Map.fromList . map swap . E.elems . view robotInventory . instantiateRobot 0 . head . view scenarioRobots

-- | Ignore utility entites that are just used for tutorials and challenges.
ignoredEntites :: Set Text
ignoredEntites =
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

-- ----------------------------------------------------------------------------
-- UTILITY
-- ----------------------------------------------------------------------------

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

guardRight :: Text -> Either Text a -> ExceptT Text IO a
guardRight what i = i `isRightOr` (\e -> "Failed to " <> what <> ": " <> e)

simpleErrorHandle :: ExceptT Text IO a -> IO a
simpleErrorHandle = either (fail . unpack) pure <=< runExceptT
