-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for styling custom entity attributes
module Swarm.Game.Scenario.Style where

import Codec.Picture (PixelRGBA8 (..))
import Data.Aeson
import Data.Colour.Palette.Types (Kolor)
import Data.Colour.SRGB (RGB (..), sRGB24read, toSRGB24)
import Data.Set (Set)
import Data.Text (Text)
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

-- | Hexadecimal color notation.
-- May include a leading hash symbol (see 'Data.Colour.SRGB.sRGB24read').
newtype HexColor = HexColor Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance ToPixel HexColor where
  toPixel (HexColor colorText) = PixelRGBA8 r g b 255
   where
    temp :: Kolor
    temp = sRGB24read $ T.unpack colorText
    RGB r g b = toSRGB24 temp

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

  conv (HexColor x) = Triple $ toSRGB24 kolor
   where
    kolor :: Kolor
    kolor = sRGB24read $ T.unpack x
