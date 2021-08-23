{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Swarm.Game.Resource where

import           Control.Lens
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Text     (Text)

import           Brick

import           Swarm.UI.Attr

data ResourceInfo = RI
  { _resourceChar :: Char
  , _resourceName :: Text
  , _resourceAttr :: AttrName
  }

makeLenses ''ResourceInfo

resourceMap :: Map Char ResourceInfo
resourceMap = M.fromList
  [ ('T', RI 'T' "Tree"   plantAttr)
  , (',', RI ',' "Grass"  plantAttr)
  , ('*', RI '*' "Flower" flowerAttr)
  , ('.', RI '.' "Dirt"   dirtAttr)
  , ('O', RI 'O' "Rock"   rockAttr)
  , (' ', RI ' ' "Air"    defAttr)
  ]

resourceList :: [Char]
resourceList = M.keys resourceMap
