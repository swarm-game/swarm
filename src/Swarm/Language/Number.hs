{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Swarm.Language.Number (
    Number(..),
) where
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Yaml (ToJSON (..))
import Swarm.Util.Yaml (FromJSONE)
import Data.Yaml.Aeson ( FromJSON(..), Value(..) )

-- | A type that represent the quantity of something.
--
-- In some situations it is useful to have _an infinite_ amount
-- of supplies in robots inventory as that is more visible
-- than hidden default devices.
--
-- However for that to work well with other language constructs
-- we introduce negative infinity as well.
data Number = NegInfinity | Count Integer | PosInfinity
  deriving (Eq, Ord, Show, Read, Generic, Data)

instance ToJSON Number where
  toJSON (Count a) = toJSON a
  toJSON NegInfinity = String "-inf"
  toJSON PosInfinity = String "inf"

instance FromJSON Number where
  parseJSON = \case
    Number s ->
      if s - fromInteger (truncate s) == 0
        then pure $ Count (truncate s)
        else fail "Count is not a whole number!"
    String "-inf" -> pure NegInfinity
    String "inf" -> pure PosInfinity
    e -> fail $
      "Expected number or null for count, but got '" <> show e <> "'!"

instance FromJSONE e Number

instance Num Number where
  (+) :: Number -> Number -> Number
  Count a + Count b = Count $ a + b
  i + Count _c = i
  Count _c + i = i
  i + j = if i == j then i else error "Can not add infinities together!"
  (*) :: Number -> Number -> Number
  Count a * Count b = Count $ a * b
  i * j = signum i * signum j * PosInfinity
  abs :: Number -> Number
  abs (Count c) = Count (abs c)
  abs NegInfinity = PosInfinity
  abs PosInfinity = PosInfinity
  signum :: Number -> Number
  signum (Count c) = Count (signum c)
  signum NegInfinity = -1
  signum PosInfinity = 1
  fromInteger :: Integer -> Number
  fromInteger = Count . fromInteger
  negate :: Number -> Number
  negate (Count c) = Count (negate c)
  negate NegInfinity = PosInfinity
  negate PosInfinity = NegInfinity  
