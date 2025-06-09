{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan Hashable instances needed to derive Hashable Display

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: TUI rendering of entities
--
-- Utilities for describing how to display in-game entities in the TUI.
module Swarm.Game.Cosmetic.Display (
  -- * The display record
  Priority,
  Display,
  Attribute,
  ChildInheritance (..),

  -- ** Fields
  defaultChar,
  orientationMap,
  displayAttr,
  displayPriority,
  invisible,
  childInheritance,

  -- ** Construction
  defaultEntityDisplay,
  defaultRobotDisplay,

  -- ** Rendering
  renderDisplay,
) where

import Control.Applicative ((<|>))
import Control.Lens hiding (Const, from, (.=))
import Control.Monad (guard, when)
import Data.Hashable (Hashable)
import Data.List.Extra (enumerate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Graphics.Text.Width
import Swarm.Game.Cosmetic.Attribute
import Swarm.Game.Cosmetic.Color (AttributeMap, TrueColor)
import Swarm.Game.Cosmetic.Texel
import Swarm.Language.Syntax.Direction (AbsoluteDir (..), Direction (..))
import Swarm.Util (applyWhen, quote)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.Yaml (FromJSONE (..), With (runE), getE, liftE, withObjectE)

data ChildInheritance
  = Invisible
  | Inherit
  | DefaultDisplay
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A record explaining how to display a robot or entity in the TUI.
data Display = Display
  { _defaultChar :: Char
  , _orientationMap :: Map AbsoluteDir Char
  , _displayAttr :: Attribute
  , _displayPriority :: Priority
  , _invisible :: Bool
  , _childInheritance :: ChildInheritance
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

makeLensesNoSigs ''Display

-- | The default character to use for display.
defaultChar :: Lens' Display Char

-- | For robots or other entities that have an orientation, this map
--   optionally associates different display characters with
--   different orientations.  If an orientation is not in the map,
--   the 'defaultChar' will be used.
orientationMap :: Lens' Display (Map AbsoluteDir Char)

-- | The attribute to use for display.
displayAttr :: Lens' Display Attribute

-- | This entity's display priority. Higher priorities are drawn
--   on top of lower.
displayPriority :: Lens' Display Priority

-- | Whether the entity is currently invisible.
invisible :: Lens' Display Bool

-- | For robots, whether children of this inherit the parent's display
childInheritance :: Lens' Display ChildInheritance

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

    liftE $ do
      let _defaultChar = c
      _orientationMap <- v .:? "orientationMap" .!= dOM
      _displayAttr <- (v .:? "attr") .!= (defD ^. displayAttr)
      _displayPriority <- v .:? "priority" .!= (defD ^. displayPriority)
      _invisible <- v .:? "invisible" .!= (defD ^. invisible)
      let _childInheritance = Inherit
      pure Display {..}
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
      ]
        ++ ["priority" .= (d ^. displayPriority) | (d ^. displayPriority) /= (defaultEntityDisplay ' ' ^. displayPriority)]
        ++ ["orientationMap" .= (d ^. orientationMap) | not (M.null (d ^. orientationMap))]
        ++ ["invisible" .= (d ^. invisible) | d ^. invisible]

-- | Construct a default display for an entity that uses only a single
--   display character, the default entity attribute, and priority 1.
defaultEntityDisplay :: Char -> Display
defaultEntityDisplay c =
  Display
    { _defaultChar = c
    , _orientationMap = M.empty
    , _displayAttr = AEntity
    , _displayPriority = 1
    , _invisible = False
    , _childInheritance = Inherit
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
    , _displayAttr = ARobot
    , _displayPriority = 10
    , _invisible = False
    , _childInheritance = Inherit
    }

-- * Boundary rendering

-- | This type is isomorphic to 'Bool' but
-- is more compact for readability of the
-- 'glyphForNeighbors' cases.
data Presence
  = -- | present
    X
  | -- | absent
    O

emptyNeighbors :: Neighbors Presence
emptyNeighbors = Neighbors O O O O

data Neighbors a = Neighbors
  { e :: a
  , w :: a
  , n :: a
  , s :: a
  }

computeNeighborPresence :: (AbsoluteDir -> Bool) -> Neighbors Presence
computeNeighborPresence checkPresence =
  foldr assignPresence emptyNeighbors enumerate
 where
  assignPresence d = applyWhen (checkPresence d) $ setNeighbor d X

setNeighbor :: AbsoluteDir -> a -> Neighbors a -> Neighbors a
setNeighbor DNorth x y = y {n = x}
setNeighbor DSouth x y = y {s = x}
setNeighbor DEast x y = y {e = x}
setNeighbor DWest x y = y {w = x}

-- | For a center cell that itself is a boundary,
-- determine a glyph override for rendering, given certain
-- neighbor combinations.
glyphForNeighbors :: Neighbors Presence -> Maybe Char
glyphForNeighbors = \case
  Neighbors {e = O, w = O, n = O, s = O} -> Nothing
  Neighbors {e = X, w = O, n = O, s = O} -> Just '╶'
  Neighbors {e = O, w = X, n = O, s = O} -> Just '╴'
  Neighbors {e = X, w = X, n = O, s = O} -> Just '─'
  Neighbors {e = O, w = O, n = X, s = O} -> Just '╵'
  Neighbors {e = O, w = O, n = O, s = X} -> Just '╷'
  Neighbors {e = O, w = O, n = X, s = X} -> Just '│'
  Neighbors {e = X, w = O, n = X, s = O} -> Just '└'
  Neighbors {e = X, w = O, n = O, s = X} -> Just '┌'
  Neighbors {e = O, w = X, n = X, s = O} -> Just '┘'
  Neighbors {e = O, w = X, n = O, s = X} -> Just '┐'
  Neighbors {e = X, w = X, n = X, s = O} -> Just '┴'
  Neighbors {e = X, w = X, n = O, s = X} -> Just '┬'
  Neighbors {e = X, w = O, n = X, s = X} -> Just '├'
  Neighbors {e = O, w = X, n = X, s = X} -> Just '┤'
  Neighbors {e = X, w = X, n = X, s = X} -> Just '┼'

getBoundaryDisplay :: (AbsoluteDir -> Bool) -> Maybe Char
getBoundaryDisplay = glyphForNeighbors . computeNeighborPresence

-- * Rendering

-- | Turn a Display into a concrete 'Texel', taking into account
--   orientation, boundaries, whether the entity is known or
--   invisible, etc.
renderDisplay :: AttributeMap -> Maybe Direction -> (Maybe AbsoluteDir -> Bool) -> Bool -> Display -> Texel TrueColor
renderDisplay aMap mdir boundaryCheck known disp
  | disp ^. invisible = mempty
  | otherwise = maybe mempty (texelFromColor (disp ^. displayPriority) c) mcolor
 where
  mhidden = guard (not known) *> pure '?'
  mbound = guard (boundaryCheck Nothing) *> getBoundaryDisplay (boundaryCheck . Just)
  morient = do
    DAbsolute d <- mdir
    M.lookup d (disp ^. orientationMap)

  c = fromMaybe (disp ^. defaultChar) $ mhidden <|> mbound <|> morient

  mcolor = M.lookup (disp ^. displayAttr) aMap
