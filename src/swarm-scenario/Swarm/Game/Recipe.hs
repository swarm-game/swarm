{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Entity transformations, not limited to "crafting"
--
-- A recipe represents some kind of process for transforming
-- some input entities into some output entities.
--
-- Recipes support a number of different game mechanics, including:
--
-- * crafting
-- * mining
-- * randomized "loot boxes"
-- * unlocking doors
--
-- == Synchronous vs Async
-- Recipes can be completed either within the same tick
-- as execution is started, or execution may span
-- multiple ticks. It is possible for the execution
-- of multi-tick recipes to be interrupted in one way or
-- another, in which case the recipe fails without producing
-- the "outputs".
module Swarm.Game.Recipe (
  -- * Ingredient lists and recipes
  IngredientList,
  Recipe (..),

  -- ** Fields
  recipeInputs,
  recipeOutputs,
  recipeCatalysts,
  recipeTime,
  recipeWeight,

  -- * Loading recipes
  loadRecipes,
  outRecipeMap,
  inRecipeMap,
  catRecipeMap,

  -- * Looking up recipes
  MissingIngredient (..),
  MissingType (..),
  knowsIngredientsFor,
  recipesFor,
  make,
  make',
  findLacking,
) where

import Control.Algebra (Has)
import Control.Arrow (left)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw, liftEither)
import Control.Lens hiding (from, (.=))
import Control.Monad ((<=<))
import Data.Bifunctor (second)
import Data.Either.Validation
import Data.Foldable (Foldable (..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Failure
import Swarm.Game.Entity as E
import Swarm.Game.Ingredients
import Swarm.ResourceLoading (getDataFileNameSafe)
import Swarm.Util.Effect (withThrow)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.Yaml
import Witch
import Prelude hiding (Foldable (..))

-- | A recipe represents some kind of process where inputs are
--   transformed into outputs.
data Recipe e = Recipe
  { _recipeInputs :: IngredientList e
  , _recipeOutputs :: IngredientList e
  , _recipeCatalysts :: IngredientList e
  , _recipeTime :: Integer
  , _recipeWeight :: Integer
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

deriving instance ToJSON (Recipe Entity)
deriving instance FromJSON (Recipe Entity)

makeLensesNoSigs ''Recipe

-- | The inputs to a recipe.
recipeInputs :: Lens' (Recipe e) (IngredientList e)

-- | The outputs from a recipe.
recipeOutputs :: Lens' (Recipe e) (IngredientList e)

-- | The time required to finish a recipe.
recipeTime :: Lens' (Recipe e) Integer

-- | Other entities which the recipe requires you to have, but which
--   are not consumed by the recipe (e.g. a @\"furnace\"@).
recipeCatalysts :: Lens' (Recipe e) (IngredientList e)

-- | How this recipe is weighted against other recipes.  Any time
--   there are multiple valid recipes that fit certain criteria, one
--   of the recipes will be randomly chosen with probability
--   proportional to its weight.
recipeWeight :: Lens' (Recipe e) Integer

------------------------------------------------------------
-- Serializing
------------------------------------------------------------

instance ToJSON (Recipe Text) where
  toJSON (Recipe ins outs cats time weight) =
    object $
      [ "in" .= ins
      , "out" .= outs
      ]
        ++ ["required" .= cats | not (null cats)]
        ++ ["time" .= time | time /= 1]
        ++ ["weight" .= weight | weight /= 1]

instance FromJSON (Recipe Text) where
  parseJSON = withObject "Recipe" $ \v -> do
    _recipeInputs <- v .: "in"
    _recipeOutputs <- v .: "out"
    _recipeCatalysts <- v .:? "required" .!= []
    _recipeTime <- v .:? "time" .!= 1
    _recipeWeight <- v .:? "weight" .!= 1
    pure Recipe {..}

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
loadRecipes ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  EntityMap ->
  m [Recipe Entity]
loadRecipes em = do
  fileName <- getDataFileNameSafe Recipes f
  textRecipes <-
    withThrow (AssetNotLoaded (Data Recipes) fileName . CanNotParseYaml)
      . (liftEither <=< sendIO)
      $ decodeFileEither @[Recipe Text] fileName
  withThrow (AssetNotLoaded (Data Recipes) fileName . CustomMessage)
    . liftEither
    . left (T.append "Unknown entities in recipe(s): " . T.intercalate ", ")
    . validationToEither
    $ resolveRecipes em textRecipes
 where
  f = "recipes.yaml"

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

-- | Build a map of recipes indexed by catalysts.
catRecipeMap :: [Recipe Entity] -> IntMap [Recipe Entity]
catRecipeMap = buildRecipeMap recipeCatalysts

-- | Get a list of all the recipes for the given entity.  Look up an
--   entity in either an 'inRecipeMap' or 'outRecipeMap' depending on
--   whether you want to know recipes that consume or produce the
--   given entity, respectively.
recipesFor :: IntMap [Recipe Entity] -> Entity -> [Recipe Entity]
recipesFor rm e = fromMaybe [] $ IM.lookup (e ^. entityHash) rm

-- | Record information about something missing from a recipe.
data MissingIngredient = MissingIngredient MissingType Count Entity
  deriving (Show, Eq)

-- | What kind of thing is missing?
data MissingType = MissingInput | MissingCatalyst
  deriving (Show, Eq)

-- | Determines whether recipe inputs are satisfied by a
-- robot's inventory.
findLacking :: Inventory -> [(Count, Entity)] -> [(Count, Entity)]
findLacking robotInventory = filter ((> 0) . fst) . map countNeeded
 where
  countNeeded (need, entity) = (need - E.lookup entity robotInventory, entity)

-- | Figure out which ingredients (if any) are lacking from an
--   inventory to be able to carry out the recipe.  Catalysts are not
--   consumed and so can be used even when equipped.
missingIngredientsFor :: (Inventory, Inventory) -> Recipe Entity -> [MissingIngredient]
missingIngredientsFor (inv, ins) (Recipe inps _ cats _ _) =
  mkMissing MissingInput (findLacking inv inps)
    <> mkMissing MissingCatalyst (findLacking ins (findLacking inv cats))
 where
  mkMissing k = map (uncurry (MissingIngredient k))

-- | Figure out if a recipe is available, /i.e./ if we at least know
--   about all the ingredients.  Note it does not matter whether we have
--   enough of the ingredients.
knowsIngredientsFor :: (Inventory, Inventory) -> Recipe Entity -> Bool
knowsIngredientsFor (inv, ins) recipe =
  knowsAll inv (recipe ^. recipeInputs) && knowsAll ins (recipe ^. recipeCatalysts)
 where
  knowsAll xs = all (E.contains xs . snd)

-- | Try to make a recipe, deleting the recipe's inputs from the
--   inventory. Return either a description of which items are
--   lacking, if the inventory does not contain sufficient inputs,
--   or an inventory without inputs and function adding outputs if
--   it was successful.
make ::
  -- | The robot's inventory and equipped devices
  (Inventory, Inventory) ->
  -- | The recipe we are trying to make
  Recipe Entity ->
  Either
    [MissingIngredient]
    (Inventory, IngredientList Entity, Recipe Entity)
make invs r = finish <$> make' invs r
 where
  finish (invTaken, out) = (invTaken, out, r)

-- | Try to make a recipe, but do not insert it yet.
make' ::
  (Inventory, Inventory) ->
  Recipe Entity ->
  Either
    [MissingIngredient]
    (Inventory, IngredientList Entity)
make' invs@(inv, _) r =
  case missingIngredientsFor invs r of
    [] ->
      let removed = foldl' (flip (uncurry deleteCount)) inv (r ^. recipeInputs)
       in Right (removed, r ^. recipeOutputs)
    missing -> Left missing
