{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Resource
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for working with /resources/, /i.e./ raw materials, which
-- can exist in the world and are represented by a certain character.
--
-----------------------------------------------------------------------------

module Swarm.Game.Resource
  ( -- * Resource info record and lenses
    ResourceProperty(..)
  , ResourceInfo(..), resName, resChar, resAttr, resProperties

    -- * Resource map

  , resourceMap, resourceList

  )
where

import           Control.Lens
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Text     (Text)

import           Brick

import           Swarm.UI.Attr

data ResourceProperty
  = Solid          -- ^ Robots can't move onto a cell with this resource.
  | Harvestable    -- ^ Robots can harvest this.
  deriving (Eq, Ord, Show)

-- | A record to hold information about a resource.
data ResourceInfo = RI
  { _resChar       :: Char     -- ^ The character used to represent
                               --   the resource in the world map.
  , _resName       :: Text     -- ^ The name of the resource, used
                               --   /e.g./ in an inventory display.
  , _resAttr       :: AttrName -- ^ The name of the @brick@ VTY
                               --   attribute to use when drawing the
                               --   resource.
  , _resProperties :: Set ResourceProperty
  }

makeLenses ''ResourceInfo

-- | A map containing info about every resource in the game.
resourceMap :: Map Char ResourceInfo
resourceMap = M.fromList
  [ ('T', RI 'T' "Tree"   plantAttr (S.singleton Harvestable))
  , (',', RI ',' "Grass"  plantAttr S.empty)
  , ('*', RI '*' "Flower" flowerAttr (S.singleton Harvestable))
  , ('.', RI '.' "Dirt"   dirtAttr S.empty)
  , ('@', RI '@' "Rock"   rockAttr (S.singleton Solid))
  ]

-- | A list of resource characters, for convenience; this is just a
--   listing of all the keys in the 'resourceMap'.
resourceList :: [Char]
resourceList = M.keys resourceMap
