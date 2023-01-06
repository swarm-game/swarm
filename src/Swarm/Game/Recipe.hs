{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
  recipeRequirements,
  recipeTime,
  recipeWeight,

  -- * Loading recipes
  loadRecipes,
  outRecipeMap,
  inRecipeMap,
  reqRecipeMap,

  -- * Looking up recipes
  MissingIngredient (..),
  MissingType (..),
  knowsIngredientsFor,
  recipesFor,
  make,
  make',
) where

import Control.Lens hiding (from, (.=))
import Data.Bifunctor (second)
import Data.Either.Validation
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Witch

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Throw.Either (runThrow, throwError)
import Swarm.Game.Entity as E
import Swarm.Util
import Swarm.Util.Yaml

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
  , _recipeRequirements :: IngredientList e
  , _recipeTime :: Integer
  , _recipeWeight :: Integer
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

deriving instance ToJSON (Recipe Entity)
deriving instance FromJSON (Recipe Entity)

makeLensesWith (lensRules & generateSignatures .~ False) ''Recipe

-- | The inputs to a recipe.
recipeInputs :: Lens' (Recipe e) (IngredientList e)

-- | The outputs from a recipe.
recipeOutputs :: Lens' (Recipe e) (IngredientList e)

-- | The time required to finish a recipe.
recipeTime :: Lens' (Recipe e) Integer

-- | Other entities which the recipe requires you to have, but which
--   are not consumed by the recipe (e.g. a furnace).
recipeRequirements :: Lens' (Recipe e) (IngredientList e)

-- | How this recipe is weighted against other recipes.  Any time
--   there are multiple valid recipes that fit certain criteria, one
--   of the recipes will be randomly chosen with probability
--   proportional to its weight.
recipeWeight :: Lens' (Recipe e) Integer

------------------------------------------------------------
-- Serializing
------------------------------------------------------------

instance ToJSON (Recipe Text) where
  toJSON (Recipe ins outs reqs time weight) =
    object $
      [ "in" .= ins
      , "out" .= outs
      ]
        ++ ["required" .= reqs | not (null reqs)]
        ++ ["time" .= time | time /= 1]
        ++ ["weight" .= weight | weight /= 1]

instance FromJSON (Recipe Text) where
  parseJSON = withObject "Recipe" $ \v ->
    Recipe
      <$> v .: "in"
      <*> v .: "out"
      <*> v .:? "required" .!= []
      <*> v .:? "time" .!= 1
      <*> v .:? "weight" .!= 1

-- | Given an 'EntityMap', turn a list of recipes containing /names/
--   of entities into a list of recipes containing actual 'Entity'
--   records; or.
resolveRecipes :: EntityMap -> [Recipe Text] -> Validation [Text] [Recipe Entity]
resolveRecipes em = (traverse . traverse) (\t -> maybe (Failure [t]) Success (lookupEntityName t em))

instance FromJSONE EntityMap (Recipe Entity) where
  parseJSONE v = do
    rt <- liftE $ parseJSON @(Recipe Text) v
    em <- getE
    let erEnt :: Validation [Text] (Recipe Entity)
        erEnt = traverse (\t -> maybe (Failure [t]) Success (lookupEntityName t em)) rt
    case validationToEither erEnt of
      Right rEnt -> return rEnt
      Left err -> fail . from @Text . T.unlines $ err

-- | Given an already loaded 'EntityMap', try to load a list of
--   recipes from the data file @recipes.yaml@.
loadRecipes :: (Has (Lift IO) sig m) => EntityMap -> m (Either Text [Recipe Entity])
loadRecipes em = runThrow $ do
  let f = "recipes.yaml"
  mayFileName <- sendIO $ getDataFileNameSafe f
  case mayFileName of
    Nothing -> sendIO (dataNotFound f) >>= throwError
    Just fileName -> do
      res <- sendIO $ decodeFileEither @[Recipe Text] fileName
      textRecipes <- res `isRightOr` (from @String @Text . prettyPrintParseException)
      resolveRecipes em textRecipes
        `isSuccessOr` (T.append "Unknown entities in recipe(s): " . T.intercalate ", ")

------------------------------------------------------------

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

-- | Build a map of recipes indexed by input ingredients.
inRecipeMap :: [Recipe Entity] -> IntMap [Recipe Entity]
inRecipeMap = buildRecipeMap recipeInputs

-- | Build a map of recipes indexed by requirements.
reqRecipeMap :: [Recipe Entity] -> IntMap [Recipe Entity]
reqRecipeMap = buildRecipeMap recipeRequirements

-- | Get a list of all the recipes for the given entity.  Look up an
--   entity in either an 'inRecipeMap' or 'outRecipeMap' depending on
--   whether you want to know recipes that consume or produce the
--   given entity, respectively.
recipesFor :: IntMap [Recipe Entity] -> Entity -> [Recipe Entity]
recipesFor rm e = fromMaybe [] $ IM.lookup (e ^. entityHash) rm

data MissingIngredient = MissingIngredient MissingType Count Entity
  deriving (Show, Eq)

data MissingType = MissingInput | MissingCatalyst
  deriving (Show, Eq)

-- | Figure out which ingredients (if any) are lacking from an
--   inventory to be able to carry out the recipe.
--   Requirements are not consumed and so can use equipped.
missingIngredientsFor :: (Inventory, Inventory) -> Recipe Entity -> [MissingIngredient]
missingIngredientsFor (inv, ins) (Recipe inps _ reqs _ _) =
  mkMissing MissingInput (findLacking inv inps)
    <> mkMissing MissingCatalyst (findLacking ins (findLacking inv reqs))
 where
  mkMissing k = map (uncurry (MissingIngredient k))
  findLacking inven = filter ((> 0) . fst) . map (countNeeded inven)
  countNeeded inven (need, entity) = (need - E.lookup entity inven, entity)

-- | Figure out if a recipe is available, but it can be lacking items.
knowsIngredientsFor :: (Inventory, Inventory) -> Recipe Entity -> Bool
knowsIngredientsFor (inv, ins) recipe =
  knowsAll inv (recipe ^. recipeInputs) && knowsAll ins (recipe ^. recipeRequirements)
 where
  knowsAll xs = all (E.contains xs . snd)

-- | Try to make a recipe, deleting the recipe's inputs from the
--   inventory. Return either a description of which items are
--   lacking, if the inventory does not contain sufficient inputs,
--   or an inventory without inputs and function adding outputs if
--   it was successful.
make ::
  -- robots inventory and equipped devices
  (Inventory, Inventory) ->
  -- considered recipe
  Recipe Entity ->
  -- failure (with count of missing) or success with a new inventory,
  -- a function to add results and the recipe repeated
  Either
    [MissingIngredient]
    (Inventory, IngredientList Entity, Recipe Entity)
make invs r = finish <$> make' invs r
 where
  finish (invTaken, out) = (invTaken, out, r)

-- | Try to make a recipe, but do not insert it yet.
make' :: (Inventory, Inventory) -> Recipe Entity -> Either [MissingIngredient] (Inventory, IngredientList Entity)
make' invs@(inv, _) r =
  case missingIngredientsFor invs r of
    [] ->
      let removed = foldl' (flip (uncurry deleteCount)) inv (r ^. recipeInputs)
       in Right (removed, r ^. recipeOutputs)
    missing -> Left missing
