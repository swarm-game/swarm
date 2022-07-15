-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan Hashable instances needed to derive Hashable Display

-- |
-- Module      :  Swarm.Game.Display
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for describing how to display in-game entities in the TUI.
module Swarm.Game.Display (
  -- * The display record
  Priority,
  Display,

  -- ** Fields
  defaultChar,
  orientationMap,
  displayAttr,
  displayPriority,

  -- ** Lookup
  lookupDisplay,
  displayWidget,

  -- ** Construction
  defaultTerrainDisplay,
  defaultEntityDisplay,
  defaultRobotDisplay,
) where

import Brick (AttrName, Widget, str, withAttr)
import Control.Lens hiding (Const, from, (.=))
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M

import Data.Yaml
import GHC.Generics (Generic)

import Swarm.Language.Syntax
import Swarm.TUI.Attr
import Swarm.Util

-- | Display priority.  Entities with higher priority will be drawn on
--   top of entities with lower priority.
type Priority = Int

-- Some orphan instances we need to be able to derive a Hashable
-- instance for Display
instance Hashable AttrName

-- | A record explaining how to display an entity in the TUI.
data Display = Display
  { _defaultChar :: Char
  , _orientationMap :: Map Direction Char
  , _displayAttr :: AttrName
  , _displayPriority :: Priority
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

makeLensesWith (lensRules & generateSignatures .~ False) ''Display

-- | The default character to use for display.
defaultChar :: Lens' Display Char

-- | For robots or other entities that have an orientation, this map
--   optionally associates different display characters with
--   different orientations.  If an orientation is not in the map,
--   the 'defaultChar' will be used.
orientationMap :: Lens' Display (Map Direction Char)

-- | The attribute to use for display.
displayAttr :: Lens' Display AttrName

-- | This entity's display priority. Higher priorities are drawn
--   on top of lower.
displayPriority :: Lens' Display Priority

instance FromJSON Display where
  parseJSON = withObject "Display" $ \v ->
    Display
      <$> v .: "char"
      <*> v .:? "orientationMap" .!= M.empty
      <*> v .:? "attr" .!= entityAttr
      <*> v .:? "priority" .!= 1

instance ToJSON Display where
  toJSON d =
    object $
      [ "char" .= (d ^. defaultChar)
      , "attr" .= (d ^. displayAttr)
      , "priority" .= (d ^. displayPriority)
      ]
        ++ ["orientationMap" .= (d ^. orientationMap) | not (M.null (d ^. orientationMap))]

-- | Look up the character that should be used for a display, possibly
--   given an orientation as input.
lookupDisplay :: Maybe Direction -> Display -> Char
lookupDisplay Nothing disp = disp ^. defaultChar
lookupDisplay (Just v) disp = M.lookup v (disp ^. orientationMap) ? (disp ^. defaultChar)

-- | Given the (optional) orientation of an entity and its display,
--   return a widget showing the entity.
displayWidget :: Maybe Direction -> Display -> Widget n
displayWidget orient disp = withAttr (disp ^. displayAttr) $ str [lookupDisplay orient disp]

-- | The default way to display some terrain using the given character
--   and attribute, with priority 0.
defaultTerrainDisplay :: Char -> AttrName -> Display
defaultTerrainDisplay c attr =
  defaultEntityDisplay c
    & displayPriority .~ 0
    & displayAttr .~ attr

-- | Construct a default display for an entity that uses only a single
--   display character, the default entity attribute, and priority 1.
defaultEntityDisplay :: Char -> Display
defaultEntityDisplay c =
  Display
    { _defaultChar = c
    , _orientationMap = M.empty
    , _displayAttr = entityAttr
    , _displayPriority = 1
    }

-- | Construct a default robot display, with display characters
--   @"X^>v<"@, the default robot attribute, and priority 10.
--
-- Note that the 'defaultChar' is used for direction 'DDown'
-- and is overriden for the special base robot.
defaultRobotDisplay :: Display
defaultRobotDisplay =
  Display
    { _defaultChar = 'X'
    , _orientationMap =
        M.fromList
          [ (DEast, '>')
          , (DWest, '<')
          , (DSouth, 'v')
          , (DNorth, '^')
          ]
    , _displayAttr = robotAttr
    , _displayPriority = 10
    }
