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
data Number = NegInfinity | Integer Integer | PosInfinity
  deriving (Eq, Ord, Show, Read, Generic, Data)

instance ToJSON Number where
  toJSON (Integer a) = toJSON a
  toJSON NegInfinity = String "-inf"
  toJSON PosInfinity = String "inf"

instance FromJSON Number where
  parseJSON = \case
    Number s ->
      if s - fromInteger (truncate s) == 0
        then pure $ Integer (truncate s)
        else fail "Integer is not a whole number!"
    String "-inf" -> pure NegInfinity
    String "inf" -> pure PosInfinity
    e -> fail $
      "Expected number or null for count, but got '" <> show e <> "'!"

instance FromJSONE e Number

instance Num Number where
  (+) :: Number -> Number -> Number
  Integer a + Integer b = Integer $ a + b
  i + Integer _c = i
  Integer _c + i = i
  i + j = if i == j then i else error "Can not add infinities together!"
  (*) :: Number -> Number -> Number
  Integer a * Integer b = Integer $ a * b
  i * j = case (signum i * signum j) `compare` 0 of
    LT -> NegInfinity
    EQ -> 0
    GT -> PosInfinity
  abs :: Number -> Number
  abs (Integer c) = Integer (abs c)
  abs NegInfinity = PosInfinity
  abs PosInfinity = PosInfinity
  signum :: Number -> Number
  signum (Integer c) = Integer (signum c)
  signum NegInfinity = -1
  signum PosInfinity = 1
  fromInteger :: Integer -> Number
  fromInteger = Integer
  negate :: Number -> Number
  negate (Integer c) = Integer (negate c)
  negate NegInfinity = PosInfinity
  negate PosInfinity = NegInfinity  
