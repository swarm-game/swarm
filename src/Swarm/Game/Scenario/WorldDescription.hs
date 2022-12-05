{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Scenario.WorldDescription where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import GHC.Int (Int64)
import Linear.V2
import Swarm.Game.Entity
import Swarm.Game.Scenario.Cells
import Swarm.Game.Scenario.RobotLookup
import Swarm.Util.Yaml
import Witch (into)

------------------------------------------------------------
-- World description
------------------------------------------------------------

-- | A world palette maps characters to 'Cell' values.
newtype WorldPalette = WorldPalette
  {unPalette :: KeyMap Cell}
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) WorldPalette where
  parseJSONE = withObjectE "palette" $ fmap WorldPalette . mapM parseJSONE

-- | A description of a world parsed from a YAML file.
data WorldDescription = WorldDescription
  { defaultTerrain :: Maybe Cell
  , offsetOrigin :: Bool
  , palette :: WorldPalette
  , ul :: V2 Int64
  , area :: [[Cell]]
  }
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> pure pal
      <*> liftE (v .:? "upperleft" .!= V2 0 0)
      <*> liftE ((v .:? "map" .!= "") >>= paintMap pal)

-- | "Paint" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'Cell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap :: MonadFail m => WorldPalette -> Text -> m [[Cell]]
paintMap pal = traverse (traverse toCell . into @String) . T.lines
 where
  toCell c = case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
    Nothing -> fail $ "Char not in world palette: " ++ show c
    Just cell -> return cell
