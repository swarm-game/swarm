-- | A model for defining boolean expressions for Objective prerequisites.
--
-- This model is intended to be user-facing in the .yaml files, and is
-- distinct from that in 'Data.BoolExpr'.
module Swarm.Game.Scenario.Objective.Logic where

import Data.Aeson
import Data.BoolExpr
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty ((:|)))
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
  parseJSON = genericParseJSON prerequisiteOptions

toBoolExpr :: Prerequisite a -> BoolExpr a
toBoolExpr (And (x :| [])) = toBoolExpr x
toBoolExpr (And (x0 :| x : xs)) = BAnd (toBoolExpr x0) (toBoolExpr $ And $ x :| xs)
toBoolExpr (Or (x :| [])) = toBoolExpr x
toBoolExpr (Or (x0 :| x : xs)) = BOr (toBoolExpr x0) (toBoolExpr $ Or $ x :| xs)
toBoolExpr (Not x) = BNot $ toBoolExpr x
toBoolExpr (Id x) = BConst $ pure x
