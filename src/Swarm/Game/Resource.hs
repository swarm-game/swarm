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
    ResourceInfo(..), resourceName, resourceChar, resourceAttr

    -- * Resource map

  , resourceMap, resourceList

  )
where

import           Control.Lens
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Text     (Text)

import           Brick

import           Swarm.UI.Attr

-- | A record to hold information about a resource.
data ResourceInfo = RI
  { _resourceChar :: Char      -- ^ The character used to represent
                               --   the resource in the world map.
  , _resourceName :: Text      -- ^ The name of the resource, used
                               --   /e.g./ in an inventory display.
  , _resourceAttr :: AttrName  -- ^ The name of the @brick@ VTY
                               --   attribute to use when drawing the
                               --   resource.
  }

makeLenses ''ResourceInfo

-- | A map containing info about every resource in the game.
resourceMap :: Map Char ResourceInfo
resourceMap = M.fromList
  [ ('T', RI 'T' "Tree"   plantAttr)
  , (',', RI ',' "Grass"  plantAttr)
  , ('*', RI '*' "Flower" flowerAttr)
  , ('.', RI '.' "Dirt"   dirtAttr)
  , ('O', RI 'O' "Rock"   rockAttr)
  , (' ', RI ' ' "Air"    defAttr)
  ]

-- | A list of resource characters, for convenience; this is just a
--   listing of all the keys in the 'resourceMap'.
resourceList :: [Char]
resourceList = M.keys resourceMap
