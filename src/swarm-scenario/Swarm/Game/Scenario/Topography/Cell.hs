{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Cell (
  PCell (..),
  Cell,
  AugmentedCell,
  CellPaintDisplay,
  cellToEntity,
) where

import Control.Lens hiding (from, (.=), (<.>))
import Control.Monad.Extra (mapMaybeM, unless)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml as Y
import Swarm.Game.Cosmetic.Display (defaultEntityDisplay)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Land
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.ProtoCell hiding (name)
import Swarm.Game.Terrain
import Swarm.Language.Syntax (Phase (..))
import Swarm.Util (quote, showT)
import Swarm.Util.Erasable (Erasable (..), erasableToMaybe)
import Swarm.Util.Yaml

------------------------------------------------------------
-- World cells
------------------------------------------------------------

-- | A single cell in a world map, which contains a terrain value,
--   and optionally an entity and a list of robots.
--   It is parameterized on the 'Entity' type to facilitate less
--   stateful versions of the 'Entity' type in rendering scenario data.
data PCell e phase = Cell
  { cellTerrain :: TerrainType
  , cellEntity :: Erasable e
  , cellRobots :: [IndexedRobot phase]
  }

-- | A single cell in a world map, which contains a terrain value,
--   and optionally an entity and robot.
type Cell = PCell Entity

-- | Supplements a cell with waypoint information
type AugmentedCell e phase = SignpostableCell (PCell e phase)

-- | Re-usable serialization for variants of 'PCell'
mkPCellJson :: ToJSON b => (Erasable a -> Maybe b) -> PCell a phase -> Value
mkPCellJson modifier x =
  toJSON $
    catMaybes
      [ Just . toJSON . getTerrainWord $ cellTerrain x
      , fmap toJSON . modifier $ cellEntity x
      , listToMaybe []
      ]

instance ToJSON (Cell phase) where
  toJSON = mkPCellJson $ \case
    EErase -> Just "erase"
    ENothing -> Nothing
    EJust e -> Just (e ^. entityName)

-- | Parse a tuple such as @[grass, rock, base]@ into a 'Cell'.  The
--   entity and robot, if present, are immediately looked up and
--   converted into 'Entity' and 'TRobot' values.  If they are not
--   found, a parse error results.
instance FromJSONE (TerrainEntityMaps, RobotMap Raw) (Cell Raw) where
  parseJSONE = withArrayE "tuple" $ \v -> do
    let tupRaw = V.toList v
    tup <- case NE.nonEmpty tupRaw of
      Nothing -> fail "palette entry must have nonzero length (terrain, optional entity and then robots if any)"
      Just x -> return x

    (TerrainEntityMaps tm _, _) <- getE
    terr <- liftE $ parseJSON (NE.head tup)
    unless (M.member terr $ terrainByName tm)
      . fail
      . T.unpack
      $ T.unwords
        [ "Unrecognized terrain type"
        , quote $ getTerrainWord terr
        , "Avaliable:"
        , showT $ M.keys $ terrainByName tm
        ]

    ent <- case tup ^? ix 1 of
      Nothing -> return ENothing
      Just e -> do
        meName <- liftE $ parseJSON @(Maybe Text) e
        case meName of
          Nothing -> return ENothing
          Just "erase" -> return EErase
          Just name -> fmap EJust . localE (view entityMap . fst) $ getEntity name

    let name2rob r = do
          mrName <- liftE $ parseJSON @(Maybe RobotName) r
          traverse (localE snd . getRobot) mrName

    robs <- mapMaybeM name2rob (drop 2 tupRaw)

    return $ Cell terr ent robs

cellToEntity :: Maybe (Cell phase) -> Maybe Entity
cellToEntity = ((erasableToMaybe . cellEntity) =<<)

------------------------------------------------------------
-- World editor
------------------------------------------------------------

-- | Stateless cells used for the World Editor.
-- These cells contain the bare minimum display information
-- for rendering.
type CellPaintDisplay = PCell EntityFacade

-- Note: This instance is used only for the purpose of 'WorldPalette'
instance ToJSON (CellPaintDisplay phase) where
  toJSON = mkPCellJson $ \case
    ENothing -> Nothing
    EErase -> Just $ EntityFacade "erase" (defaultEntityDisplay ' ') Nothing
    EJust e -> Just e
