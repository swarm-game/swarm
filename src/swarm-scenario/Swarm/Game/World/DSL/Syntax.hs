{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for the Swarm world description DSL.
module Swarm.Game.World.DSL.Syntax (
  -- | Various component types
  World,
  RawCellVal,
  CellTag (..),
  CellVal (..),
  Var,
  Axis (..),
  Op (..),
  -- | The main AST type
  WExp (..),
)
where

import Control.Lens (view, (^.))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Swarm.Game.Entity (Entity, entityName)
import Swarm.Game.Robot (TRobot, trobotName)
import Swarm.Game.Terrain
import Swarm.Game.World.Coords
import Swarm.Pretty (PrettyPrec (..))
import Swarm.Util (showT)
import Swarm.Util.Erasable

------------------------------------------------------------
-- Bits and bobs

type World b = Coords -> b

data CellTag = CellTerrain | CellEntity | CellRobot
  deriving (Eq, Ord, Show, Enum, Bounded)

instance PrettyPrec CellTag where
  prettyPrec _ = \case
    CellTerrain -> "terrain"
    CellEntity -> "an entity"
    CellRobot -> "a robot"

type RawCellVal = [(Maybe CellTag, Text)]

prettyRawCellItem :: (Maybe CellTag, Text) -> Doc ann
prettyRawCellItem (Nothing, t) = pretty t
prettyRawCellItem (Just tag, t) = pretty (T.toLower . T.drop 4 . showT $ tag) <> ":" <> pretty t

data CellVal = CellVal TerrainType (Erasable (Last Entity)) [TRobot]
  deriving (Eq, Show)

instance PrettyPrec CellVal where
  prettyPrec _ (CellVal terr ent rs) =
    "{" <> hsep (punctuate "," (map prettyRawCellItem items)) <> "}"
   where
    items =
      [(Just CellTerrain, getTerrainWord terr) | terr /= BlankT]
        ++ [(Just CellEntity, e ^. entityName) | EJust (Last e) <- [ent]]
        ++ map ((Just CellRobot,) . view trobotName) rs

type Var = Text

data Axis = X | Y
  deriving (Eq, Ord, Show, Bounded, Enum)

instance PrettyPrec Axis where
  prettyPrec _ = \case X -> "x"; Y -> "y"

data Op = Not | Neg | And | Or | Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | If | Perlin | Mask | Overlay | Abs | IMap
  deriving (Eq, Ord, Show)

------------------------------------------------------------
-- Main AST

data WExp where
  WInt :: Integer -> WExp
  WFloat :: Double -> WExp
  WBool :: Bool -> WExp
  WCell :: RawCellVal -> WExp
  WVar :: Text -> WExp
  -- Require all operators to be fully saturated.  Just embedding
  -- operators as constants and including function application would
  -- be a more elegant encoding, but it requires being more clever
  -- with type inference.
  WOp :: Op -> [WExp] -> WExp
  WSeed :: WExp
  WCoord :: Axis -> WExp
  WHash :: WExp
  WLet :: [(Var, WExp)] -> WExp -> WExp
  WOverlay :: NE.NonEmpty WExp -> WExp
  WImport :: Text -> WExp
  deriving (Eq, Show)

-- We don't have an explicit Empty case because we can't infer its
-- type.  It could be done but it would require a lot more care with
-- inference vs checking mode.

-- TODO (#1394): Add hcat and vcat operations
-- WCat :: Axis -> [WExp] -> WExp

-- TODO (#1394): Add support for structures
-- WStruct :: WorldPalette Text -> [Text] -> WExp
