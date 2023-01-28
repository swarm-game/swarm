module Swarm.Game.Scenario.Style where

import Brick (AttrName, attrName)
import Data.Aeson
import Data.Set (Set, toList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Graphics.Vty.Attributes
import Numeric (readHex)
import Swarm.TUI.Attr (worldPrefix)

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

toStyle :: StyleFlag -> Style
toStyle = \case
  Standout -> standout
  Italic -> italic
  Strikethrough -> strikethrough
  Underline -> underline
  ReverseVideo -> reverseVideo
  Blink -> blink
  Dim -> dim
  Bold -> bold

styleFlagJsonOptions :: Options
styleFlagJsonOptions =
  defaultOptions
    { sumEncoding = UntaggedValue
    }

instance FromJSON StyleFlag where
  parseJSON = genericParseJSON styleFlagJsonOptions

newtype HexColor = HexColor Text
  deriving (Eq, Show, Generic, FromJSON)

data CustomAttr = CustomAttr
  { name :: String
  , fg :: Maybe HexColor
  , bg :: Maybe HexColor
  , style :: Maybe (Set StyleFlag)
  }
  deriving (Eq, Show, Generic, FromJSON)

toAttrColor :: HexColor -> Color
toAttrColor (HexColor colorText) =
  case nums of
    [r, g, b] -> RGBColor r g b
    _ -> RGBColor 255 255 255
 where
  chunks = T.chunksOf 2 colorText
  nums = map (fst . head . readHex . T.unpack) chunks

toAttrPair :: CustomAttr -> (AttrName, Attr)
toAttrPair ca =
  (worldPrefix <> attrName (name ca), addStyle $ addFg $ addBg currentAttr)
 where
  addFg = maybe id (flip withForeColor . toAttrColor) $ fg ca
  addBg = maybe id (flip withBackColor . toAttrColor) $ bg ca
  addStyle = maybe id (flip withStyle . sum . map toStyle . toList) $ style ca
