module Swarm.Game.Scenario.Logic where

import Data.Aeson
import Data.BoolExpr
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup
import Data.Text (Text)
import GHC.Generics (Generic)

type ObjectiveLabel = Text

-- | This is only needed for constructing a Graph,
-- which requires all nodes to have a key.
data ObjectiveId
  = Label (Signed ObjectiveLabel)
  | -- | for unlabeled objectives
    Ordinal Int
  deriving (Eq, Ord, Show)

-- | In contrast with the "BoolExpr" type,
-- "And" and "Or" can have zero or more children
-- instead of exactly two.
data Prerequisite a
  = And (NonEmpty (Prerequisite a))
  | Or (NonEmpty (Prerequisite a))
  | Not (Prerequisite a)
  | Id a
  deriving (Eq, Show, Generic, Functor, Foldable)

-- | This may not be needed, due to 'boolexpr'
met :: Prerequisite Bool -> Bool
met (And x) = getAll $ sconcat $ fmap (All . met) x
met (Or x) = getAny $ sconcat $ fmap (Any . met) x
met (Not x) = not $ met x
met (Id x) = x

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
