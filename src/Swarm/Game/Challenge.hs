{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Swarm.Game.Challenge
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Challenges are standalone worlds with specific starting and winning
-- conditions, which can be used both for building interactive
-- tutorials and for standalone puzzles and scenarios.
module Swarm.Game.Challenge (
  -- * The Challenge type
  Challenge (..),
  ChallengeRecord,

  -- ** Fields
  challengeName,
  challengeSeed,
  challengeWorld,
  challengeRobots,
  challengeWin,

  -- * A sample challenge (for testing)
  sampleChallenge,
) where

import Control.Arrow ((***))
import Control.Lens
import Data.Array
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Linear.V2

import Swarm.Game.Entity
import Swarm.Game.Recipe (Recipe)
import Swarm.Game.Robot (Robot, baseRobot, robotLocation)
import Swarm.Game.Terrain
import Swarm.Game.World
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)

-- | A challenge can be instantiated to a 'ChallengeRecord' once we
--   have loaded entities and recipes.
newtype Challenge = Challenge (EntityMap -> [Recipe Entity] -> ChallengeRecord)

-- | A 'ChallengeRecord' contains all the information to describe a
--   challenge.
data ChallengeRecord = ChallengeRecord
  { _challengeName :: Text
  , _challengeSeed :: Maybe Int
  , _challengeWorld :: WorldFun Int Entity
  , _challengeRobots :: [Robot]
  , _challengeWin :: ProcessedTerm
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''ChallengeRecord

-- | The name of the challenge.
challengeName :: Lens' ChallengeRecord Text

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed.
challengeSeed :: Lens' ChallengeRecord (Maybe Int)

-- | The starting world for the challenge.
challengeWorld :: Lens' ChallengeRecord (WorldFun Int Entity)

-- | The starting robots for the challenge.  Note this should
--   include the "base".
challengeRobots :: Lens' ChallengeRecord [Robot]

-- | The winning condition for the challenge, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
challengeWin :: Lens' ChallengeRecord ProcessedTerm

-- | A sample challenge.
sampleChallenge :: Challenge
sampleChallenge = Challenge $ \em _ ->
  ChallengeRecord
    { _challengeName = "Sample challenge"
    , _challengeSeed = Just 0
    , _challengeWorld = worldFunFromArray (arr em) (fromEnum StoneT, Nothing)
    , _challengeRobots =
        [baseRobot (startDevices em) & robotLocation .~ V2 0 0]
    , _challengeWin =
        [tmQ| loc <- getRobotLoc "base";
              return (loc == inr (4,4))
            |]
    }
 where
  startDevices em = mapMaybe (`lookupEntityName` em) ["treads"]

  arr em =
    listArray ((0, 0), (4, 4)) . map (toEntity em) . concat $
      [ "  T  "
      , " T   "
      , "     "
      , "   T "
      , "T    "
      ]
  toEntity em c = (fromEnum *** (`lookupEntityName` em)) $ case c of
    'T' -> (DirtT, "tree")
    _ -> (StoneT, "nothing")
