-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Structure.Recognition.Static where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Language.Syntax.Direction (AbsoluteDir)

data RotationalSymmetry
  = -- | Aka 1-fold symmetry
    NoSymmetry
  | -- | Equivalent under rotation by 180 degrees
    TwoFold
  | -- | Equivalent under rotation by 90 degrees
    FourFold
  deriving (Show, Eq)

data SymmetryAnnotatedGrid a = SymmetryAnnotatedGrid
  { symmetry :: RotationalSymmetry
  , namedGrid :: a
  }
  deriving (Show, Functor, Foldable, Traversable)

data OrientedStructure = OrientedStructure
  { oName :: StructureName
  , oDir :: AbsoluteDir
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)

-- | For use in registering recognizable pre-placed structures.
--
-- Compare to
-- 'Swarm.Game.Scenario.Topography.Structure.Recognition.Type.PositionedStructure'.
data LocatedStructure = LocatedStructure
  { placedStruct :: OrientedStructure
  , cornerLoc :: Location
  }
  deriving (Show)

instance HasLocation LocatedStructure where
  modifyLoc f (LocatedStructure x originalLoc) =
    LocatedStructure x $ f originalLoc
