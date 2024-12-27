{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for styling custom entity attributes
module Swarm.Game.Scenario.Style where

import Codec.Picture (PixelRGBA8 (..))
import Data.Aeson
import Data.Colour.Palette.Types (Kolor)
import Data.Colour.SRGB (RGB (..), sRGB24reads, toSRGB24, sRGB24show)
import Data.Colour.SRGB.Linear (toRGB)
import Data.Set (Set)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Scenario.Topography.Rasterize

data StyleFlag
  = Standout
  | Italic
  | Strikethrough
  | Underline
  | ReverseVideo
  | Blink
  | Dim
  | Bold
  deriving (Eq, Ord, Show, Generic)

styleFlagJsonOptions :: Options
styleFlagJsonOptions =
  defaultOptions
    { sumEncoding = UntaggedValue
    }

instance FromJSON StyleFlag where
  parseJSON = genericParseJSON styleFlagJsonOptions

instance ToJSON StyleFlag where
  toJSON = genericToJSON styleFlagJsonOptions

-- | A color, parsed from hexadecimal notation.  May include a leading
--   hash symbol (see 'Data.Colour.SRGB.sRGB24read').
newtype HexColor = HexColor { getHexColor :: Kolor }
  deriving (Eq, Show, Generic)

instance Ord HexColor where
  -- There is no Ord instance for Colour a, but we need one to use
  -- with OccurrenceEncoder, so we make our own.
  --
  -- We use toRGB here since it does no conversions whatsoever, it
  -- simply unpacks the raw color data into an RGB triple.  For the
  -- purposes of an Ord instance, it doesn't matter: we just want a
  -- consistent way to put a total ordering on colors as fast as
  -- possible.
  compare (HexColor (toRGB -> RGB r1 g1 b1)) (HexColor (toRGB -> RGB r2 g2 b2))
    = compare r1 r2 <> compare g1 g2 <> compare b1 b2

instance FromJSON HexColor where
  parseJSON = withText "hex color" $ \t ->
    case sRGB24reads (T.unpack t) of
      ((c, _):_) -> pure $ HexColor c
      _ -> fail $ "Could not parse hex color '" ++ T.unpack t ++ "'"

instance ToJSON HexColor where
  toJSON = toJSON . T.pack . sRGB24show . getHexColor

instance ToPixel HexColor where
  toPixel (HexColor kolor) = PixelRGBA8 r g b 255
   where
    RGB r g b = toSRGB24 kolor

data CustomAttr = CustomAttr
  { name :: String
  , fg :: Maybe HexColor
  , bg :: Maybe HexColor
  , style :: Maybe (Set StyleFlag)
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON CustomAttr where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        }

-- | Must specify either a foreground or background color;
-- just a style is not sufficient.
toHifiPair :: CustomAttr -> Maybe (WorldAttr, PreservableColor)
toHifiPair (CustomAttr n maybeFg maybeBg _) =
  sequenceA (WorldAttr n, fmap conv <$> c)
 where
  c = case (maybeFg, maybeBg) of
    (Just f, Just b) -> Just $ FgAndBg f b
    (Just f, Nothing) -> Just $ FgOnly f
    (Nothing, Just b) -> Just $ BgOnly b
    (Nothing, Nothing) -> Nothing

  conv (HexColor kolor) = Triple $ toSRGB24 kolor
