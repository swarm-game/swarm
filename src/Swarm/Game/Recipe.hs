-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Recipe
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A recipe represents some kind of crafting process for transforming
-- some input entities into some output entities.  This module both
-- defines the 'Recipe' type and also defines the master list of
-- recipes for the game.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.Game.Recipe where

import           Control.Lens
import           Data.Bifunctor    (second)
import           Data.IntMap       (IntMap)
import qualified Data.IntMap       as IM
import           Data.List         (foldl')
import           Data.Maybe        (fromMaybe, listToMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Witch

import           Swarm.Game.Entity (Count, Entity, Inventory)
import qualified Swarm.Game.Entity as E
import           Swarm.Util

-- | An ingredient list is a list of entities with multiplicity.
type IngredientList = [(Count, Entity)]

-- | A recipe is just a list of input entities and a list of output
--   entities (both with multiplicity).  The idea is that it
--   represents some kind of \"crafting\" process where the inputs are
--   transformed into the outputs.
data Recipe = Recipe
  { _recipeInputs  :: IngredientList
  , _recipeOutputs :: IngredientList
  }
  deriving (Eq, Ord, Show)

makeLenses ''Recipe

prettyRecipe :: Recipe -> Text
prettyRecipe (Recipe ins outs) =
  T.concat [ prettyIngredientList ins, " -> ", prettyIngredientList outs ]

prettyIngredientList :: IngredientList -> Text
prettyIngredientList = T.intercalate " + " . map prettyIngredient
  where
    prettyIngredient (n,e) = T.concat [ into @Text (show n), " ", number n (e ^. E.entityName) ]

buildRecipeMap :: Getter Recipe IngredientList -> IntMap [Recipe]
buildRecipeMap select = IM.fromListWith (++) (map (second (:[])) (concatMap mk recipeList))
  where
    mk r = [(e ^. E.entityHash, r) | (_, e) <- r ^. select]

-- | A map of recipes indexed by output ingredients. Built
--   automatically from the 'recipeList'.
outRecipeMap :: IntMap [Recipe]
outRecipeMap = buildRecipeMap recipeOutputs

-- | Get a list of all the recipes which have the given entity as an
--   output.
recipesFor :: Entity -> [Recipe]
recipesFor e = fromMaybe [] $ IM.lookup (e ^. E.entityHash) outRecipeMap

-- | Look up a recipe for crafting a specific entity.
recipeFor :: Entity -> Maybe Recipe
recipeFor = listToMaybe . recipesFor

-- | A map of recipes indexed by input ingredients. Built
--   automatically from the 'recipeList'.
inRecipeMap :: IntMap [Recipe]
inRecipeMap = buildRecipeMap recipeInputs

-- | Get a list of all the recipes which have the given entity as an input.
recipesUsing :: Entity -> [Recipe]
recipesUsing e = fromMaybe [] $ IM.lookup (e ^. E.entityHash) inRecipeMap

-- | Get a list of all the recipes which have the given entity as either an input or output.
--   The ones using the entity as an output are listed first.
recipesWith :: Entity -> [Recipe]
recipesWith e = recipesFor e ++ recipesUsing e

-- | Figure out which ingredients (if any) are lacking from an
--   inventory to be able to carry out the recipe.
missingIngredientsFor :: Inventory -> Recipe -> [(Count, Entity)]
missingIngredientsFor inv (Recipe ins _)
  = filter ((>0) . fst) $ map (\(n,e) -> (n - E.lookup e inv, e)) ins

-- | Try to craft a recipe, deleting the recipe's inputs from the
--   inventory and adding the outputs. Return either a description of
--   which items are lacking, if the inventory does not contain
--   sufficient inputs, or an updated inventory if it was successful.
craft :: Recipe -> Inventory -> Either [(Count, Entity)] Inventory
craft r@(Recipe ins outs) inv = case missingIngredientsFor inv r of
  []      -> Right $
    foldl' (flip (uncurry E.insertCount)) (foldl' (flip (uncurry E.deleteCount)) inv ins) outs
  missing -> Left missing

-- | A big old list of all the recipes in the game.
recipeList :: [Recipe]
recipeList = []
  -- [ Recipe
  --   [(1, E.tree)]
  --   [(2, E.branch), (1, E.log)]

  -- , Recipe
  --   [(1, E.log)]
  --   [(4, E.wood)]

  -- , Recipe
  --   [(6, E.wood)]
  --   [(1, E.box)]

  -- , Recipe
  --   [(2, E.wood)]
  --   [(1, E.gear)]

  -- , Recipe
  --   [(1, E.bit False), (1, E.bit True)]
  --   [(1, E.drillBit)]
  -- ]
