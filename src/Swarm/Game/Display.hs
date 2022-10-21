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
  curOrientation,
  displayAttr,
  displayPriority,
  invisible,

  -- ** Rendering
  displayChar,
  renderDisplay,
  hidden,

  -- ** Construction
  defaultTerrainDisplay,
  defaultEntityDisplay,
  defaultRobotDisplay,
) where

import Brick (AttrName, Widget, str, withAttr)
import Control.Lens hiding (Const, from, (.=))
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Language.Syntax (Direction (..))
import Swarm.TUI.Attr (entityAttr, robotAttr, worldPrefix)
import Swarm.Util (maxOn, (?))
import Swarm.Util.Yaml (FromJSONE(..), withObjectE, getE, liftE, With (runE))

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
  , _curOrientation :: Maybe Direction
  , _displayAttr :: AttrName
  , _displayPriority :: Priority
  , _invisible :: Bool
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

instance Semigroup Display where
  d1 <> d2
    | _invisible d1 = d2
    | _invisible d2 = d1
    | otherwise = maxOn _displayPriority d1 d2

makeLensesWith (lensRules & generateSignatures .~ False) ''Display

-- | The default character to use for display.
defaultChar :: Lens' Display Char

-- | For robots or other entities that have an orientation, this map
--   optionally associates different display characters with
--   different orientations.  If an orientation is not in the map,
--   the 'defaultChar' will be used.
orientationMap :: Lens' Display (Map Direction Char)

-- | The display caches the current orientation of the entity, so we
--   know which character to use from the orientation map.
curOrientation :: Lens' Display (Maybe Direction)

-- | The attribute to use for display.
displayAttr :: Lens' Display AttrName

-- | This entity's display priority. Higher priorities are drawn
--   on top of lower.
displayPriority :: Lens' Display Priority

-- | Whether the entity is currently invisible.
invisible :: Lens' Display Bool

instance FromJSON Display where
  parseJSON v = runE (parseJSONE v) (defaultEntityDisplay ' ')

instance FromJSONE Display Display where
  parseJSONE = withObjectE "Display" $ \v -> do
    defD <- getE
    liftE $ Display
      <$> v .:? "char" .!= (defD ^. defaultChar)
      <*> v .:? "orientationMap" .!= (defD ^. orientationMap)
      <*> v .:? "curOrientation" .!= (defD ^. curOrientation)
      <*> (fmap (worldPrefix <>) <$> v .:? "attr") .!= (defD ^. displayAttr)
      <*> v .:? "priority" .!= (defD ^. displayPriority)
      <*> v .:? "invisible" .!= (defD ^. invisible)


instance ToJSON Display where
  toJSON d =
    object $
      [ "char" .= (d ^. defaultChar)
      , "attr" .= (d ^. displayAttr)
      , "priority" .= (d ^. displayPriority)
      ]
        ++ ["orientationMap" .= (d ^. orientationMap) | not (M.null (d ^. orientationMap))]
        ++ ["invisible" .= (d ^. invisible) | d ^. invisible]

-- | Look up the character that should be used for a display.
displayChar :: Display -> Char
displayChar disp = case disp ^. curOrientation of
  Nothing -> disp ^. defaultChar
  Just dir -> M.lookup dir (disp ^. orientationMap) ? (disp ^. defaultChar)

-- | Render a display as a UI widget.
renderDisplay :: Display -> Widget n
renderDisplay disp = withAttr (disp ^. displayAttr) $ str [displayChar disp]

-- | Modify a display to use a @?@ character for entities that are
--   hidden/unknown.
hidden :: Display -> Display
hidden = (defaultChar .~ '?') . (curOrientation .~ Nothing)

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
    , _curOrientation = Nothing
    , _displayAttr = entityAttr
    , _displayPriority = 1
    , _invisible = False
    }

-- | Construct a default robot display for a given orientation, with
--   display characters @"X^>v<"@, the default robot attribute, and
--   priority 10.
--
--   Note that the 'defaultChar' is used for direction 'DDown'
--   and is overriden for the special base robot.
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
    , _curOrientation = Nothing
    , _displayAttr = robotAttr
    , _displayPriority = 10
    , _invisible = False
    }

instance Monoid Display where
  mempty = defaultEntityDisplay ' ' & invisible .~ True
