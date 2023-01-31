{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Scenario.WorldDescription where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Location
import Swarm.Game.Util.Yaml
import Witch (into)

------------------------------------------------------------
-- World description
------------------------------------------------------------

-- | A world palette maps characters to 'Cell' values.
newtype WorldPalette e = WorldPalette
  {unPalette :: KeyMap (PCell e)}
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) (WorldPalette Entity) where
  parseJSONE = withObjectE "palette" $ fmap WorldPalette . mapM parseJSONE

-- | A description of a world parsed from a YAML file.
-- This type is parameterized to accommodate Cells that
-- utilize a less stateful Entity type.
data PWorldDescription e = WorldDescription
  { defaultTerrain :: Maybe (PCell e)
  , offsetOrigin :: Bool
  , palette :: WorldPalette e
  , ul :: Location
  , area :: [[PCell e]]
  }
  deriving (Eq, Show)

type WorldDescription = PWorldDescription Entity

instance FromJSONE (EntityMap, RobotMap) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> pure pal
      <*> liftE (v .:? "upperleft" .!= origin)
      <*> liftE ((v .:? "map" .!= "") >>= paintMap pal)

-- | "Paint" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'Cell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap :: MonadFail m => WorldPalette e -> Text -> m [[PCell e]]
paintMap pal = traverse (traverse toCell . into @String) . T.lines
 where
  toCell c = case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
    Nothing -> fail $ "Char not in world palette: " ++ show c
    Just cell -> return cell
