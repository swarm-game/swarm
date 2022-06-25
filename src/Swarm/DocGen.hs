{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Swarm.DocGen (
  generateDocs,
  GenerateDocs (..),
) where

import Control.Lens (view, (^.))
import Control.Monad (zipWithM, zipWithM_, (<=<))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityName, loadEntities)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Recipe (Recipe, loadRecipes, recipeInputs, recipeOutputs, recipeRequirements)
import Swarm.Game.Robot (installedDevices, robotInventory, setRobotID)
import Swarm.Game.Scenario (Scenario, loadScenario, scenarioRobots)
import Swarm.Game.WorldGen (testWorld2Entites)
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
  deriving (Eq, Show)

generateDocs :: GenerateDocs -> IO ()
generateDocs = \case
  RecipeGraph -> generateRecipe >>= putStrLn

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
  -- finally draw recipes
  let recipeInOut r = [(snd i, snd o) | i <- r ^. recipeInputs, o <- r ^. recipeOutputs]
      recipeReqOut r = [(snd q, snd o) | q <- r ^. recipeRequirements, o <- r ^. recipeOutputs]
      recipesToPairs f rs = both nid <$> nubOrd (concatMap f rs)
  mapM_ (uncurry (.->.)) (recipesToPairs recipeInOut recipes)
  mapM_ (uncurry (---<>)) (recipesToPairs recipeReqOut recipes)

-- ----------------------------------------------------------------------------
-- RECIPE LEVELS
-- ----------------------------------------------------------------------------

-- | Order entites in sets depending on how soon it is possible to obtain them.
--
-- So:
--  * Level 0 - starting entites (for example those obtainable in the world)
--  * Level N+1 - everything possible to make (or drill) from Level N
--
-- This is almost a BFS, but the requirement is that the set of entites
-- required for recipe is subset of the entites known in Level N.
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

-- | Get classic scenario to figure out starting entites.
classicScenario :: ExceptT Text IO Scenario
classicScenario = do
  entities <- loadEntities >>= guardRight "load entities"
  loadScenario "data/scenarios/classic.yaml" entities

startingDevices :: Scenario -> Set Entity
startingDevices = Set.fromList . map snd . E.elems . view installedDevices . setRobotID 0 . head . view scenarioRobots

startingInventory :: Scenario -> Map Entity Int
startingInventory = Map.fromList . map swap . E.elems . view robotInventory . setRobotID 0 . head . view scenarioRobots

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
