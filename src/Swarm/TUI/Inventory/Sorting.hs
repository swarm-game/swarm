{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Inventory.Sorting (
  InventorySortOptions (..),
  InventorySortDirection (..),
  InventorySortOrder (..),
  cycleSortOrder,
  cycleSortDirection,
  defaultSortOptions,
  sortInventory,
  renderSortMethod,
) where

import Algorithms.NaturalSort (sortKey)
import Control.Lens (view)
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import Data.Text qualified as T
import Swarm.Game.Entity as E
import Swarm.Util (cycleEnum)

data InventorySortDirection
  = Ascending
  | Descending
  deriving (Enum, Bounded, Eq)

data InventorySortOrder
  = ByNaturalAlphabetic
  | ByQuantity
  | ByType
  deriving (Enum, Bounded, Eq)

data InventorySortOptions = InventorySortOptions InventorySortDirection InventorySortOrder

defaultSortOptions :: InventorySortOptions
defaultSortOptions = InventorySortOptions Ascending ByNaturalAlphabetic

renderSortMethod :: InventorySortOptions -> T.Text
renderSortMethod (InventorySortOptions direction order) =
  T.unwords [prefix, label]
 where
  prefix = case direction of
    Ascending -> "↑"
    Descending -> "↓"
  label = case order of
    ByNaturalAlphabetic -> "name"
    ByQuantity -> "count"
    ByType -> "type"

cycleSortOrder :: InventorySortOptions -> InventorySortOptions
cycleSortOrder (InventorySortOptions direction order) =
  InventorySortOptions direction (cycleEnum order)

cycleSortDirection :: InventorySortOptions -> InventorySortOptions
cycleSortDirection (InventorySortOptions direction order) =
  InventorySortOptions (cycleEnum direction) order

-- | All non-alphabetic sort criteria perform alphabetic tie-breaking.
-- "Reverse ordering" only applies to the *primary* sort criteria; the secondary
-- alphabetic sort is always in ascending order.
getSortCompartor :: Ord a => InventorySortOptions -> (a, Entity) -> (a, Entity) -> Ordering
getSortCompartor (InventorySortOptions direction order) = case order of
  ByNaturalAlphabetic -> compReversible (alphabetic . snd)
  ByQuantity -> compReversible fst <> secondary
  ByType -> compReversible (view entityProperties . snd) <> secondary
 where
  alphabetic = sortKey . T.toLower . view entityName
  secondary = comparing (alphabetic . snd)

  compReversible :: Ord a => (b -> a) -> b -> b -> Ordering
  compReversible = case direction of
    Ascending -> comparing
    Descending -> \f -> comparing (Down . f)

sortInventory :: Ord a => InventorySortOptions -> [(a, Entity)] -> [(a, Entity)]
sortInventory opts =
  sortBy $ getSortCompartor opts
