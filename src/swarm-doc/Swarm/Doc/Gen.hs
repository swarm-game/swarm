{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Auto-generation of various forms of documentation.
module Swarm.Doc.Gen (
  -- ** Main document generation function + types
  generateDocs,
  GenerateDocs (..),
  SheetType (..),

  -- ** Wiki pages
  PageAddress (..),

  -- ** Recipe graph data
  RecipeGraphData (..),
  EdgeFilter (..),
  classicScenarioRecipeGraphData,
  ignoredEntities,
) where

import Control.Lens (view, (^.))
import Control.Monad (zipWithM, zipWithM_)
import Data.Aeson.Text (encodeToLazyText)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List qualified as List
import Data.List.Extra (enumerate)
import Data.Map.Lazy (Map, (!))
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.Tuple (swap)
import Swarm.Doc.Command (getCatalog)
import Swarm.Doc.Keyword
import Swarm.Doc.Pedagogy
import Swarm.Doc.Util
import Swarm.Doc.Wiki.Cheatsheet
import Swarm.Failure (simpleErrorHandle)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityName, entityYields)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Land
import Swarm.Game.Recipe (Recipe, recipeCatalysts, recipeInputs, recipeOutputs)
import Swarm.Game.Robot (Robot, equippedDevices, robotInventory)
import Swarm.Game.Scenario (GameStateInputs (..), ScenarioInputs (..), loadStandaloneScenario, scenarioLandscape)
import Swarm.Game.World.Gen (extractEntities)
import Swarm.Game.World.Typecheck (Some (..))
import Swarm.Language.Key (specialKeyNames)
import Swarm.Util (both)
import Text.Dot (Dot, NodeId, (.->.))
import Text.Dot qualified as Dot

-- ============================================================================
-- MAIN ENTRYPOINT TO CLI DOCUMENTATION GENERATOR
-- ============================================================================
--
-- These are the exported functions used by the executable.
--
-- ----------------------------------------------------------------------------

-- | An enumeration of the kinds of documentation we can generate.
data GenerateDocs where
  -- | Entity dependencies by recipes.
  RecipeGraph :: EdgeFilter -> GenerateDocs
  -- | Keyword lists for editors.
  EditorKeywords :: Maybe EditorType -> GenerateDocs
  -- | List of special key names recognized by 'Swarm.Language.Syntax.Key' command
  SpecialKeyNames :: GenerateDocs
  -- | Cheat sheets for inclusion on the Swarm wiki.
  CheatSheet :: PageAddress -> SheetType -> GenerateDocs
  -- | JSON representation of commands metadata matrix
  CommandsData :: GenerateDocs
  -- | List command introductions by tutorial
  TutorialCoverage :: GenerateDocs
  deriving (Eq, Show)

-- | Generate the requested kind of documentation to stdout.
generateDocs :: GenerateDocs -> IO ()
generateDocs = \case
  RecipeGraph ef -> generateRecipe ef >>= putStrLn
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
        mapM_ editorGen enumerate
  SpecialKeyNames -> generateSpecialKeyNames
  CheatSheet address s -> makeWikiPage address s
  CommandsData -> TL.putStrLn $ encodeToLazyText getCatalog
  TutorialCoverage -> renderTutorialProgression >>= putStrLn . T.unpack

-- ----------------------------------------------------------------------------
-- GENERATE KEYWORDS: LIST OF WORDS TO BE HIGHLIGHTED
-- ----------------------------------------------------------------------------

-- | Generate a list of keywords in the format expected by one of the
--   supported editors.
generateEditorKeywords :: EditorType -> IO ()
generateEditorKeywords = \case
  Emacs -> do
    putStrLn "(defvar swarm-mode-builtins '("
    T.putStr $ builtinFunctionList Emacs <> "))"
    putStrLn "\n(defvar swarm-mode-commands '("
    T.putStr $ keywordsCommands Emacs
    T.putStr $ keywordsDirections Emacs <> "))"
    putStrLn "\n (defvar swarm-mode-operators '("
    T.putStr $ operatorNames Emacs <> "))"
  VSCode -> do
    putStrLn "Functions and commands:"
    T.putStrLn $ builtinFunctionList VSCode <> "|" <> keywordsCommands VSCode
    putStrLn "\nDirections:"
    T.putStrLn $ keywordsDirections VSCode
    putStrLn "\nOperators:"
    T.putStrLn $ operatorNames VSCode
  Vim -> do
    putStrLn "syn keyword Builtins "
    T.putStr $ builtinFunctionList Vim
    putStrLn "\nsyn keyword Command "
    T.putStr $ keywordsCommands Vim
    putStrLn "\nsyn keyword Direction "
    T.putStr $ keywordsDirections Vim
    putStrLn "\nsyn match Operators "
    T.putStr $ "[" <> operatorNames Vim <> "]"

-- ----------------------------------------------------------------------------
-- GENERATE SPECIAL KEY NAMES
-- ----------------------------------------------------------------------------

generateSpecialKeyNames :: IO ()
generateSpecialKeyNames =
  T.putStr . T.unlines . Set.toList $ specialKeyNames

-- ----------------------------------------------------------------------------
-- GENERATE GRAPHVIZ: ENTITY DEPENDENCIES BY RECIPES
-- ----------------------------------------------------------------------------

generateRecipe :: EdgeFilter -> IO String
generateRecipe ef = do
  graphData <- classicScenarioRecipeGraphData
  return . Dot.showDot $ recipesToDot graphData ef

data EdgeFilter = NoFilter | FilterForward | FilterNext
  deriving (Eq, Show)

filterEdge :: EdgeFilter -> Int -> Int -> Bool
filterEdge ef i o = case ef of
  NoFilter -> True
  FilterForward -> i <= o
  FilterNext -> i + 1 == o

recipesToDot :: RecipeGraphData -> EdgeFilter -> Dot ()
recipesToDot graphData ef = do
  Dot.attribute ("rankdir", "LR")
  Dot.attribute ("ranksep", "2")
  world <- diamond "World"
  base <- diamond "Base"
  -- --------------------------------------------------------------------------
  -- add nodes with for all the known entities
  let enames' = map (view entityName) . toList $ rgAllEntities graphData
      enames = filter (`Set.notMember` ignoredEntities) enames'
  ebmap <- Map.fromList . zip enames <$> mapM (box . unpack) enames
  -- --------------------------------------------------------------------------
  -- getters for the NodeId based on entity name or the whole entity
  let safeGetEntity m e = fromMaybe (error $ show e <> " is not an entity!?") $ m Map.!? e
      getE = safeGetEntity ebmap
      nid = getE . view entityName
  -- --------------------------------------------------------------------------
  -- Get the starting inventories, entities present in the world and compute
  -- how hard each entity is to get - see 'recipeLevels'.
  let devs = rgStartingDevices graphData
      inv = rgStartingInventory graphData
      worldEntities = rgWorldEntities graphData
      levels = rgLevels graphData
      recipes = rgRecipes graphData
  -- --------------------------------------------------------------------------
  -- Base inventory
  (_bc, ()) <- Dot.cluster $ do
    Dot.attribute ("style", "filled")
    Dot.attribute ("color", "lightgrey")
    mapM_ ((base ---<>) . nid) devs
    mapM_ ((base .->.) . nid) inv
  -- --------------------------------------------------------------------------
  -- World entities
  (_wc, ()) <- Dot.cluster $ do
    Dot.attribute ("style", "filled")
    Dot.attribute ("color", "forestgreen")
    mapM_ (uncurry (Dot..->.) . (world,) . nid) worldEntities
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
  bottom <- wrapBelowAbove worldEntities
  ls <- zipWithM subLevel [1 ..] (drop 1 levels)
  let invisibleLine = zipWithM_ (.~>.)
  tls <- mapM (const hiddenNode) levels
  bls <- mapM (const hiddenNode) levels
  invisibleLine tls bls
  invisibleLine bls (drop 1 tls)
  let sameBelowAbove (b1, t1) (b2, t2) = Dot.same [b1, b2] >> Dot.same [t1, t2]
  zipWithM_ sameBelowAbove (bottom : ls) (zip bls tls)
  -- --------------------------------------------------------------------------
  -- add node for the world and draw a line to each entity found in the wild
  -- finally draw recipes
  let eFilter = filterEdge ef
      lvl e = fromMaybe (-1) $ List.findIndex (Set.member e) levels
      recipeInOut r = [(i, o) | (_, i) <- r ^. recipeInputs, (_, o) <- r ^. recipeOutputs, lvl i `eFilter` lvl o]
      recipeReqOut r = [(q, o) | (_, q) <- r ^. recipeCatalysts, (_, o) <- r ^. recipeOutputs, lvl q `eFilter` lvl o]
      recipesToPairs f rs = both nid <$> nubOrd (concatMap f rs)
  mapM_ (uncurry (.->.)) (recipesToPairs recipeInOut recipes)
  mapM_ (uncurry (---<>)) (recipesToPairs recipeReqOut recipes)
  -- --------------------------------------------------------------------------
  -- also draw an edge for each entity that "yields" another entity
  let yieldPairs = mapMaybe (\e -> (e ^. entityName,) <$> (e ^. entityYields)) . toList $ rgAllEntities graphData
  mapM_ (uncurry (.-<>.)) (both getE <$> yieldPairs)

data RecipeGraphData = RecipeGraphData
  { rgWorldEntities :: Set Entity
  , rgStartingDevices :: Set Entity
  , rgStartingInventory :: Set Entity
  , rgLevels :: [Set Entity]
  , rgAllEntities :: Set Entity
  , rgRecipes :: [Recipe Entity]
  }

classicScenarioRecipeGraphData :: IO RecipeGraphData
classicScenarioRecipeGraphData = simpleErrorHandle $ do
  (classic, GameStateInputs (ScenarioInputs worlds (TerrainEntityMaps _ emap)) recipes) <-
    loadStandaloneScenario "data/scenarios/classic.yaml"
  baseRobot <- instantiateBaseRobot (classic ^. scenarioLandscape)
  let classicTerm = worlds ! "classic"
  let devs = startingDevices baseRobot
  let inv = Map.keysSet $ startingInventory baseRobot
  let worldEntities = case classicTerm of Some _ t -> extractEntities t
  return
    RecipeGraphData
      { rgStartingDevices = devs
      , rgStartingInventory = inv
      , rgWorldEntities = worldEntities
      , rgLevels = recipeLevels emap recipes (Set.unions [worldEntities, devs, inv])
      , rgAllEntities = Set.fromList . Map.elems $ entitiesByName emap
      , rgRecipes = recipes
      }

-- ----------------------------------------------------------------------------
-- RECIPE LEVELS
-- ----------------------------------------------------------------------------

-- | Order entities in sets depending on how soon it is possible to obtain them.
--
-- So:
--  * Level 0 - starting entities (for example those obtainable in the world)
--  * Level N+1 - everything possible to make (or drill or harvest) from Level N
--
-- This is almost a BFS, but the requirement is that the set of entities
-- required for recipe is subset of the entities known in Level N.
--
-- If we ever depend on some graph library, this could be rewritten
-- as some BFS-like algorithm with added recipe nodes, but you would
-- need to enforce the condition that recipes need ALL incoming edges.
recipeLevels :: EntityMap -> [Recipe Entity] -> Set Entity -> [Set Entity]
recipeLevels emap recipes start = levels
 where
  recipeParts r = ((r ^. recipeInputs) <> (r ^. recipeCatalysts), r ^. recipeOutputs)
  m :: [(Set Entity, Set Entity)]
  m = map (both (Set.fromList . map snd) . recipeParts) recipes
  levels :: [Set Entity]
  levels = reverse $ go [start] start
   where
    isKnown known (i, _o) = null $ i Set.\\ known
    lookupYield e = case view entityYields e of
      Nothing -> e
      Just yn -> case E.lookupEntityName yn emap of
        Nothing -> error "unknown yielded entity"
        Just ye -> ye
    yielded = Set.map lookupYield
    nextLevel known = Set.unions $ yielded known : map snd (filter (isKnown known) m)
    go ls known =
      let n = nextLevel known Set.\\ known
       in if null n
            then ls
            else go (n : ls) (Set.union n known)

startingDevices :: Robot -> Set Entity
startingDevices = Set.fromList . map snd . E.elems . view equippedDevices

startingInventory :: Robot -> Map Entity Int
startingInventory = Map.fromList . map swap . E.elems . view robotInventory

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
    , "left and vertical wall"
    , "up and horizontal wall"
    , "right and vertical wall"
    , "down and horizontal wall"
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

-- | Edge for yielded entities.
(.-<>.) :: NodeId -> NodeId -> Dot ()
e1 .-<>. e2 = Dot.edge e1 e2 attrs
 where
  attrs = [("arrowhead", "diamond"), ("color", "purple")]

-- | Hidden edge - used for layout.
(.~>.) :: NodeId -> NodeId -> Dot ()
i .~>. j = Dot.edge i j [("style", "invis")]

-- | Edge for recipe requirements and outputs.
(---<>) :: NodeId -> NodeId -> Dot ()
e1 ---<> e2 = Dot.edge e1 e2 attrs
 where
  attrs = [("arrowhead", "diamond"), ("color", "blue")]
