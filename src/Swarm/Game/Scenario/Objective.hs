{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game.Scenario.Objective where

import Control.Lens hiding (from, (<.>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (reflow)

------------------------------------------------------------
-- Scenario objectives
------------------------------------------------------------

-- | An objective is a condition to be achieved by a player in a
--   scenario.
data Objective = Objective
  { _objectiveGoal :: NonEmpty Text
  , _objectiveCondition :: ProcessedTerm
  }
  deriving (Eq, Show, Generic, ToJSON)

makeLensesWith (lensRules & generateSignatures .~ False) ''Objective

-- | An explanation of the goal of the objective, shown to the player
--   during play.  It is represented as a list of paragraphs.
objectiveGoal :: Lens' Objective (NonEmpty Text)

-- | A winning condition for the objective, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
objectiveCondition :: Lens' Objective ProcessedTerm

instance FromJSON Objective where
  parseJSON = withObject "objective" $ \v ->
    Objective
      <$> (fmap . NE.map) reflow (v .: "goal")
      <*> (v .: "condition")
