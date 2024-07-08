-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Ingredients (
  IngredientList,
  Count,
  getCost,
) where

-- | A convenient synonym to remind us when an 'Int' is supposed to
--   represent /how many/ of something we have.
type Count = Int

-- | An ingredient list is a list of entities with multiplicity.  It
--   is polymorphic in the entity type so that we can use either
--   entity names when serializing, or actual entity objects while the
--   game is running.
type IngredientList e = [(Count, e)]

getCost :: IngredientList e -> Int
getCost = sum . map fst
