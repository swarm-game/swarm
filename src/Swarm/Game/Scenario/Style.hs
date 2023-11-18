-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for styling custom entity attributes
module Swarm.Game.Scenario.Style where

import Data.Aeson
import Data.Colour.SRGB (RGB (..), sRGB24read, toSRGB24)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Game.Entity.Cosmetic

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
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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

-- | TODO Fail if neither fg nor bg are provided
toHifiPair :: CustomAttr -> (WorldAttr, HiFiColor)
toHifiPair (CustomAttr n maybeFg maybeBg _) =
  (WorldAttr n, c)
 where
  c = case (maybeFg, maybeBg) of
    (Just f, Just b) -> FgAndBg (conv f) (conv b)
    (Just f, Nothing) -> FgOnly (conv f)
    (Nothing, Just b) -> BgOnly (conv b)
    (Nothing, Nothing) -> BgOnly $ RGB 0 0 0

  conv (HexColor x) = toSRGB24 . sRGB24read $ T.unpack x
