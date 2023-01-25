-- | A model for defining boolean expressions for Objective prerequisites.
--
-- This model is intended to be user-facing in the .yaml files, and is
-- distinct from that in 'Data.BoolExpr'.
module Swarm.Game.Scenario.Objective.Logic where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.BoolExpr
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

type ObjectiveLabel = Text

-- | In contrast with the "BoolExpr" type,
-- "And" and "Or" can have /one or more/ children
-- instead of /exactly two/.
data Prerequisite a
  = And (NonEmpty (Prerequisite a))
  | Or (NonEmpty (Prerequisite a))
  | Not (Prerequisite a)
  | Id a
  deriving (Eq, Show, Generic, Functor, Foldable)

prerequisiteOptions :: Options
prerequisiteOptions =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    , constructorTagModifier = map toLower
    }

instance ToJSON (Prerequisite ObjectiveLabel) where
  toJSON = genericToJSON prerequisiteOptions

instance FromJSON (Prerequisite ObjectiveLabel) where
  parseJSON x = preString x <|> genericParseJSON prerequisiteOptions x
   where
    preString = withText "prerequisite" $ pure . Id

toBoolExpr :: Prerequisite a -> BoolExpr a
toBoolExpr (And xs) = foldr1 BAnd (fmap toBoolExpr xs)
toBoolExpr (Or xs) = foldr1 BOr (fmap toBoolExpr xs)
toBoolExpr (Not x) = BNot $ toBoolExpr x
toBoolExpr (Id x) = BConst $ pure x
