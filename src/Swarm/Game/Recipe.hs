-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Swarm.Game.Recipe
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A recipe represents some kind of process for transforming
-- some input entities into some output entities.
module Swarm.Game.Recipe (
  -- * Ingredient lists and recipes
  IngredientList,
  Recipe (..),
  recipeInputs,
  recipeOutputs,
  prettyRecipe,

  -- * Loading recipes
  loadRecipes,
  outRecipeMap,
  inRecipeMap,

  -- * Looking up recipes
  recipesFor,
  make,
) where

import Control.Lens hiding (from, (.=))
import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Either.Validation
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Witch

import Data.Yaml

import Paths_swarm
import Swarm.Game.Entity as E
import Swarm.Util

-- | An ingredient list is a list of entities with multiplicity.  It
--   is polymorphic in the entity type so that we can use either
--   entity names when serializing, or actual entity objects while the
--   game is running.
type IngredientList e = [(Count, e)]

-- | A recipe is just a list of input entities and a list of output
--   entities (both with multiplicity).  The idea is that it
--   represents some kind of process where the inputs are
--   transformed into the outputs.
data Recipe e = Recipe
  { _recipeInputs :: IngredientList e
  , _recipeOutputs :: IngredientList e
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLensesWith (lensRules & generateSignatures .~ False) ''Recipe

-- | The inputs to a recipe.
recipeInputs :: Lens' (Recipe e) (IngredientList e)

-- | The outputs from a recipe.
recipeOutputs :: Lens' (Recipe e) (IngredientList e)

------------------------------------------------------------
-- Serializing
------------------------------------------------------------

instance ToJSON (Recipe Text) where
  toJSON (Recipe ins outs) =
    object
      [ "in" .= ins
      , "out" .= outs
      ]

instance FromJSON (Recipe Text) where
  parseJSON = withObject "Recipe" $ \v ->
    Recipe <$> v .: "in" <*> v .: "out"

-- | Given an 'EntityMap', turn a list of recipes containing /names/
--   of entities into a list of recipes containing actual 'Entity'
--   records; or.
resolveRecipes :: EntityMap -> [Recipe Text] -> Validation [Text] [Recipe Entity]
resolveRecipes em = (traverse . traverse) (\t -> maybe (Failure [t]) Success (lookupEntityName t em))

-- | Given an already loaded 'EntityMap', try to load a list of
--   recipes from the data file @recipes.yaml@.
loadRecipes :: MonadIO m => EntityMap -> m (Either Text [Recipe Entity])
loadRecipes em = runExceptT $ do
  fileName <- liftIO $ getDataFileName "recipes.yaml"
  res <- liftIO $ decodeFileEither @[Recipe Text] fileName
  textRecipes <- res `isRightOr` (from . prettyPrintParseException)
  resolveRecipes em textRecipes
    `isSuccessOr` (T.append "Unknown entities in recipe(s): " . T.intercalate ", ")

------------------------------------------------------------

-- | Pretty-print a recipe in the form @input1 + input2 -> output1 + output2@.
prettyRecipe :: Recipe Entity -> Text
prettyRecipe (Recipe ins outs) =
  T.concat [prettyIngredientList ins, " -> ", prettyIngredientList outs]

-- | Pretty-print an ingredient list in the form @n1 input1 + n2 input2 + ...@
prettyIngredientList :: IngredientList Entity -> Text
prettyIngredientList = T.intercalate " + " . map prettyIngredient
 where
  prettyIngredient (n, e) = T.concat [into @Text (show n), " ", e ^. entityNameFor n]

-- | Build a map of recipes either by inputs or outputs.
buildRecipeMap ::
  Getter (Recipe Entity) (IngredientList Entity) ->
  [Recipe Entity] ->
  IntMap [Recipe Entity]
buildRecipeMap select recipeList =
  IM.fromListWith (++) (map (second (: [])) (concatMap mk recipeList))
 where
  mk r = [(e ^. entityHash, r) | (_, e) <- r ^. select]

-- | Build a map of recipes indexed by output ingredients.
outRecipeMap :: [Recipe Entity] -> IntMap [Recipe Entity]
outRecipeMap = buildRecipeMap recipeOutputs

-- | Get a list of all the recipes for the given entity.  Look up an
--   entity in either an 'inRecipeMap' or 'outRecipeMap' depending on
--   whether you want to know recipes that consume or produce the
--   given entity, respectively.
recipesFor :: IntMap [Recipe Entity] -> Entity -> [Recipe Entity]
recipesFor rm e = fromMaybe [] $ IM.lookup (e ^. entityHash) rm

-- | Build a map of recipes indexed by input ingredients.
inRecipeMap :: [Recipe Entity] -> IntMap [Recipe Entity]
inRecipeMap = buildRecipeMap recipeInputs

-- | Figure out which ingredients (if any) are lacking from an
--   inventory to be able to carry out the recipe.
missingIngredientsFor :: Inventory -> Recipe Entity -> [(Count, Entity)]
missingIngredientsFor inv (Recipe ins _) =
  filter ((> 0) . fst) $ map (\(n, e) -> (n - E.lookup e inv, e)) ins

-- | Try to make a recipe, deleting the recipe's inputs from the
--   inventory and adding the outputs. Return either a description of
--   which items are lacking, if the inventory does not contain
--   sufficient inputs, or an updated inventory if it was successful.
make :: Inventory -> Recipe Entity -> Either [(Count, Entity)] Inventory
make inv r@(Recipe ins outs) = case missingIngredientsFor inv r of
  [] ->
    Right $
      foldl' (flip (uncurry insertCount)) (foldl' (flip (uncurry deleteCount)) inv ins) outs
  missing -> Left missing
