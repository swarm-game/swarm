{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Structure.Recognition.Static where

import Control.Lens (Lens')
import Data.Map (Map)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Placement (StructureName)
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Universe (SubworldName)
import Swarm.Language.Syntax.Direction (AbsoluteDir)
import Swarm.Util.Lens (makeLensesNoSigs)

data RotationalSymmetry
  = -- | Aka 1-fold symmetry
    NoSymmetry
  | -- | Equivalent under rotation by 180 degrees
    TwoFold
  | -- | Equivalent under rotation by 90 degrees
    FourFold
  deriving (Show, Eq)

data SymmetryAnnotatedGrid a = SymmetryAnnotatedGrid
  { namedGrid :: NamedGrid a
  , symmetry :: RotationalSymmetry
  }
  deriving (Show)

-- | For use in registering recognizable pre-placed structures
data LocatedStructure = LocatedStructure
  { placedName :: StructureName
  , upDirection :: AbsoluteDir
  , cornerLoc :: Location
  }
  deriving (Show)

instance HasLocation LocatedStructure where
  modifyLoc f (LocatedStructure x y originalLoc) =
    LocatedStructure x y $ f originalLoc

data StaticStructureInfo b = StaticStructureInfo
  { _structureDefs :: [SymmetryAnnotatedGrid (Maybe b)]
  , _staticPlacements :: Map SubworldName [LocatedStructure]
  }
  deriving (Show)

makeLensesNoSigs ''StaticStructureInfo

-- | Structure templates that may be auto-recognized when constructed
-- by a robot
structureDefs :: Lens' (StaticStructureInfo b) [SymmetryAnnotatedGrid (Maybe b)]

-- | A record of the static placements of structures, so that they can be
-- added to the "recognized" list upon scenario initialization
staticPlacements :: Lens' (StaticStructureInfo b) (Map SubworldName [LocatedStructure])
