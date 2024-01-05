{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan Hashable instances needed to derive Hashable Display

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: TUI rendering of entities
--
-- Utilities for describing how to display in-game entities in the TUI.
module Swarm.Game.Display (
  -- * The display record
  Priority,
  Attribute (..),
  Display,

  -- ** Fields
  defaultChar,
  orientationMap,
  curOrientation,
  displayAttr,
  displayPriority,
  displayObscured,
  invisible,

  -- ** Rendering
  displayChar,
  hidden,

  -- ** Construction
  defaultTerrainDisplay,
  defaultEntityDisplay,
  defaultRobotDisplay,
) where

import Control.Lens hiding (Const, from, (.=))
import Control.Monad (when)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Graphics.Text.Width
import Swarm.Language.Syntax (AbsoluteDir (..), Direction (..))
import Swarm.Util (maxOn, quote)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.Yaml (FromJSONE (..), With (runE), getE, liftE, withObjectE)

-- | Display priority.  Entities with higher priority will be drawn on
--   top of entities with lower priority.
type Priority = Int

-- | An internal attribute name.
data Attribute = ADefault | ARobot | AEntity | AWorld Text | ATerrain Text
  deriving (Eq, Ord, Show, Generic, Hashable)

terrainPrefix :: Text
terrainPrefix = "terrain_"

instance FromJSON Attribute where
  parseJSON =
    withText "attribute" $
      pure . \case
        "robot" -> ARobot
        "entity" -> AEntity
        "default" -> ADefault
        t | terrainPrefix `T.isPrefixOf` t -> ATerrain $ T.drop (T.length terrainPrefix) t
        w -> AWorld w

instance ToJSON Attribute where
  toJSON = \case
    ADefault -> String "default"
    ARobot -> String "robot"
    AEntity -> String "entity"
    AWorld w -> String w
    ATerrain t -> String $ terrainPrefix <> t

-- | A record explaining how to display an entity in the TUI.
data Display = Display
  { _defaultChar :: Char
  , _orientationMap :: Map AbsoluteDir Char
  , _curOrientation :: Maybe Direction
  , _displayAttr :: Attribute
  , _displayPriority :: Priority
  , _displayObscured :: Bool
  , _invisible :: Bool
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

instance Semigroup Display where
  d1 <> d2
    | _invisible d1 = d2
    | _invisible d2 = d1
    | otherwise = maxOn _displayPriority d1 d2

makeLensesNoSigs ''Display

-- | The default character to use for display.
defaultChar :: Lens' Display Char

-- | For robots or other entities that have an orientation, this map
--   optionally associates different display characters with
--   different orientations.  If an orientation is not in the map,
--   the 'defaultChar' will be used.
orientationMap :: Lens' Display (Map AbsoluteDir Char)

-- | The display caches the current orientation of the entity, so we
--   know which character to use from the orientation map.
curOrientation :: Lens' Display (Maybe Direction)

-- | The attribute to use for display.
displayAttr :: Lens' Display Attribute

-- | This entity's display priority. Higher priorities are drawn
--   on top of lower.
displayPriority :: Lens' Display Priority

-- | True for static "fog of war" overlay. This field is a workaround to allow
-- robot-occupied cells to take on ambient background; it distinguishes
-- displays that have an adoptable background from displays that do not.
displayObscured :: Lens' Display Bool

-- | Whether the entity is currently invisible.
invisible :: Lens' Display Bool

instance FromJSON Display where
  parseJSON v = runE (parseJSONE v) (defaultEntityDisplay ' ')

instance FromJSONE Display Display where
  parseJSONE = withObjectE "Display" $ \v -> do
    defD <- getE
    mc <- liftE $ v .:? "char"

    let c = fromMaybe (defD ^. defaultChar) mc
    validateChar c

    let dOM = if isJust mc then mempty else defD ^. orientationMap
    mapM_ validateChar $ M.elems dOM

    liftE $
      Display c
        <$> v .:? "orientationMap" .!= dOM
        <*> v .:? "curOrientation" .!= (defD ^. curOrientation)
        <*> (v .:? "attr") .!= (defD ^. displayAttr)
        <*> v .:? "priority" .!= (defD ^. displayPriority)
        <*> pure False
        <*> v .:? "invisible" .!= (defD ^. invisible)
   where
    validateChar c =
      when (charWidth > 1)
        . fail
        . T.unpack
        $ T.unwords
          [ "Character"
          , quote $ T.singleton c
          , "is too wide:"
          , T.pack $ show charWidth
          ]
     where
      charWidth = safeWcwidth c

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
displayChar disp = fromMaybe (disp ^. defaultChar) $ do
  DAbsolute d <- disp ^. curOrientation
  M.lookup d (disp ^. orientationMap)

-- | Modify a display to use a @?@ character for entities that are
--   hidden/unknown.
hidden :: Display -> Display
hidden = (defaultChar .~ '?') . (curOrientation .~ Nothing)

-- | The default way to display some terrain using the given character
--   and attribute, with priority 0.
defaultTerrainDisplay :: Attribute -> Display
defaultTerrainDisplay attr =
  defaultEntityDisplay ' '
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
    , _displayAttr = AEntity
    , _displayPriority = 1
    , _displayObscured = False
    , _invisible = False
    }

-- | Construct a default robot display for a given orientation, with
--   display characters @"X^>v<"@, the default robot attribute, and
--   priority 10.
--
--   Note that the 'defaultChar' is used for direction 'DDown'
--   and is overridden for the special base robot.
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
    , _displayAttr = ARobot
    , _displayPriority = 10
    , _displayObscured = False
    , _invisible = False
    }

instance Monoid Display where
  mempty = defaultEntityDisplay ' ' & invisible .~ True
