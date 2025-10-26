-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Structure.Named where

import Data.Set (Set)
import Data.Text (Text)
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Language.Syntax.Direction (AbsoluteDir)

newtype StructureName = StructureName { getStructureName :: Text }
  deriving Generic
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

data NamedArea a = NamedArea
  { name :: StructureName
  , recognize :: Set AbsoluteDir
  -- ^ whether this structure should be registered for automatic recognition
  -- and which orientations shall be recognized.
  -- The supplied direction indicates which cardinal direction the
  -- original map's "North" has been re-oriented to.
  -- E.g., 'DWest' represents a rotation of 90 degrees counter-clockwise.
  , description :: Maybe Text
  -- ^ will be UI-facing only if this is a recognizable structure
  , structure :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

isRecognizable :: NamedArea a -> Bool
isRecognizable = not . null . recognize
