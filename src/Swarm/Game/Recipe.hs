-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Recipe
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A recipe represents some kind of crafting process for transforming
-- some input entities into some output entities.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.Game.Recipe where

import           Control.Lens           hiding (from, (.=))
import           Data.Bifunctor         (second)
import           Data.Either.Validation
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IM
import           Data.List              (foldl')
import           Data.Map               (Map)
import           Data.Maybe             (fromMaybe, listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Witch

import           Data.Yaml

import           Control.Monad.Except
import qualified Data.Map               as M
import           Paths_swarm
import           Swarm.Game.Entity      (Count, Entity, Inventory)
import qualified Swarm.Game.Entity      as E
import           Swarm.Util

-- | An ingredient list is a list of entities with multiplicity.  It
--   is polymorphic in the entity type so that we can use either
--   entity names when serializing, or actual entity objects while the
--   game is running.
type IngredientList e = [(Count, e)]

-- | A recipe is just a list of input entities and a list of output
--   entities (both with multiplicity).  The idea is that it
--   represents some kind of \"crafting\" process where the inputs are
--   transformed into the outputs.
data Recipe e = Recipe
  { _recipeInputs  :: IngredientList e
  , _recipeOutputs :: IngredientList e
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''Recipe

------------------------------------------------------------
-- Serializing
------------------------------------------------------------

instance ToJSON (Recipe Text) where
  toJSON (Recipe ins outs) = object
    [ "in"  .= ins
    , "out" .= outs
    ]

instance FromJSON (Recipe Text) where
  parseJSON = withObject "Recipe" $ \v ->
    Recipe <$> v .: "in" <*> v .: "out"

resolveRecipes :: Map Text Entity -> [Recipe Text] -> Validation [Text] [Recipe Entity]
resolveRecipes em = (traverse . traverse) (\t -> maybe (Failure [t]) Success (M.lookup t em))

loadRecipes :: MonadIO m => Map Text Entity -> m (Either Text [Recipe Entity])
loadRecipes em = runExceptT $ do
    fileName <- liftIO $ getDataFileName "recipes.yaml"
    res <- liftIO $ decodeFileEither @[Recipe Text] fileName
    textRecipes <- res `isRightOr` (from . prettyPrintParseException)
    resolveRecipes em textRecipes `isSuccessOr`
      (T.append "Unknown entities in recipe(s): " . T.intercalate ", ")

------------------------------------------------------------

prettyRecipe :: Recipe Entity -> Text
prettyRecipe (Recipe ins outs) =
  T.concat [ prettyIngredientList ins, " -> ", prettyIngredientList outs ]

prettyIngredientList :: IngredientList Entity -> Text
prettyIngredientList = T.intercalate " + " . map prettyIngredient
  where
    prettyIngredient (n,e) = T.concat [ into @Text (show n), " ", number n (e ^. E.entityName) ]

buildRecipeMap
  :: Getter (Recipe Entity) (IngredientList Entity)
  -> [Recipe Entity] -> IntMap [Recipe Entity]
buildRecipeMap select recipeList =
  IM.fromListWith (++) (map (second (:[])) (concatMap mk recipeList))
  where
    mk r = [(e ^. E.entityHash, r) | (_, e) <- r ^. select]

-- | Create a map of recipes indexed by output ingredients.
outRecipeMap :: [Recipe Entity] -> IntMap [Recipe Entity]
outRecipeMap = buildRecipeMap recipeOutputs

-- | Get a list of all the recipes for the given entity.
recipesFor :: IntMap [Recipe Entity] -> Entity -> [Recipe Entity]
recipesFor rm e = fromMaybe [] $ IM.lookup (e ^. E.entityHash) rm

-- | Look up a recipe for crafting a specific entity.
recipeFor :: IntMap [Recipe Entity] -> Entity -> Maybe (Recipe Entity)
recipeFor rm = listToMaybe . recipesFor rm

-- | Build a map of recipes indexed by input ingredients.
inRecipeMap :: [Recipe Entity] -> IntMap [Recipe Entity]
inRecipeMap = buildRecipeMap recipeInputs

-- -- -- | Get a list of all the recipes which have the given entity as either an input or output.
-- -- --   The ones using the entity as an output are listed first.
-- recipesWith :: Entity -> [Recipe Entity]
-- recipesWith = undefined
-- -- recipesWith e = recipesFor e ++ recipesUsing e

-- | Figure out which ingredients (if any) are lacking from an
--   inventory to be able to carry out the recipe.
missingIngredientsFor :: Inventory -> Recipe Entity -> [(Count, Entity)]
missingIngredientsFor inv (Recipe ins _)
  = filter ((>0) . fst) $ map (\(n,e) -> (n - E.lookup e inv, e)) ins

-- | Try to craft a recipe, deleting the recipe's inputs from the
--   inventory and adding the outputs. Return either a description of
--   which items are lacking, if the inventory does not contain
--   sufficient inputs, or an updated inventory if it was successful.
craft :: Recipe Entity -> Inventory -> Either [(Count, Entity)] Inventory
craft r@(Recipe ins outs) inv = case missingIngredientsFor inv r of
  []      -> Right $
    foldl' (flip (uncurry E.insertCount)) (foldl' (flip (uncurry E.deleteCount)) inv ins) outs
  missing -> Left missing
