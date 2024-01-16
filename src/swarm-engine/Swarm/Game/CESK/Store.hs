{-# LANGUAGE DeriveGeneric #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Store for Swarm's CESK interpreter
module Swarm.Game.CESK.Store (
  Store,
  Addr,
  emptyStore,
  MemCell (..),
  allocate,
  lookupStore,
  setStore,
  resetBlackholes,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import GHC.Generics (Generic)

import Swarm.Language.Syntax
import Swarm.Language.Value as V

type Addr = Int

-- | 'Store' represents a store, /i.e./ memory, indexing integer
--   locations to 'MemCell's.
data Store = Store {next :: Addr, mu :: IntMap MemCell}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A memory cell can be in one of three states.
data MemCell
  = -- | A cell starts out life as an unevaluated term together with
    --   its environment.
    E Term Env
  | -- | When the cell is 'Force'd, it is set to a 'Blackhole' while
    --   being evaluated.  If it is ever referenced again while still
    --   a 'Blackhole', that means it depends on itself in a way that
    --   would trigger an infinite loop, and we can signal an error.
    --   (Of course, we
    --   <http://www.lel.ed.ac.uk/~gpullum/loopsnoop.html cannot
    --   detect /all/ infinite loops this way>.)
    --
    --   A 'Blackhole' saves the original 'Term' and 'Env' that are
    --   being evaluated; if Ctrl-C is used to cancel a computation
    --   while we are in the middle of evaluating a cell, the
    --   'Blackhole' can be reset to 'E'.
    Blackhole Term Env
  | -- | Once evaluation is complete, we cache the final 'Value' in
    --   the 'MemCell', so that subsequent lookups can just use it
    --   without recomputing anything.
    V Value
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

emptyStore :: Store
emptyStore = Store 0 IM.empty

-- | Allocate a new memory cell containing an unevaluated expression
--   with the current environment.  Return the index of the allocated
--   cell.
allocate :: Env -> Term -> Store -> (Addr, Store)
allocate e t (Store n m) = (n, Store (n + 1) (IM.insert n (E t e) m))

-- | Look up the cell at a given index.
lookupStore :: Addr -> Store -> Maybe MemCell
lookupStore n = IM.lookup n . mu

-- | Set the cell at a given index.
setStore :: Addr -> MemCell -> Store -> Store
setStore n c (Store nxt m) = Store nxt (IM.insert n c m)

-- | Reset any 'Blackhole's in the 'Store'.  We need to use this any
--   time a running computation is interrupted, either by an exception
--   or by a Ctrl+C.
resetBlackholes :: Store -> Store
resetBlackholes (Store n m) = Store n (IM.map resetBlackhole m)
 where
  resetBlackhole (Blackhole t e) = E t e
  resetBlackhole c = c
