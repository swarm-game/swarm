{-# LANGUAGE OverloadedStrings #-}
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
  Challenge,

  -- ** Fields
  challengeName,
  challengeWorld,
  challengeRobots,
  challengeWin,

  -- * A sample challenge (for testing)
  sampleChallenge,
) where

import Control.Lens
import Data.Array
import Data.Text (Text)

import Control.Arrow ((***))
import Linear.V2
import Swarm.Game.Entity
import Swarm.Game.Robot (Robot, baseRobot, robotLocation)
import Swarm.Game.Terrain
import Swarm.Game.World
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)

data Challenge = Challenge
  { _challengeName :: Text
  , _challengeWorld :: WorldFun Int Entity
  , _challengeRobots :: [Robot]
  , _challengeWin :: ProcessedTerm
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''Challenge

-- | The name of the challenge.
challengeName :: Lens' Challenge Text

-- | The starting world for the challenge.
challengeWorld :: Lens' Challenge (WorldFun Int Entity)

-- | The starting robots for the challenge.  Note this should
--   include the "base".
challengeRobots :: Lens' Challenge [Robot]

-- | The winning condition for the challenge, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
challengeWin :: Lens' Challenge ProcessedTerm

-- | A sample challenge.
sampleChallenge :: EntityMap -> Challenge
sampleChallenge em =
  Challenge
    { _challengeName = "Sample challenge"
    , _challengeWorld = worldFunFromArray arr (fromEnum StoneT, Nothing)
    , _challengeRobots =
        [baseRobot [] & robotLocation .~ V2 2 2]
    , _challengeWin =
        [tmQ| return false |]
        -- We will need to add some powerful "God-like" sensing
        -- commands to be used only for challenge win conditions...
    }
 where
  arr =
    listArray ((0, 0), (5, 5)) . map toEntity . concat $
      [ "  T  "
      , " T   "
      , "     "
      , "   T "
      , "T    "
      ]
  toEntity c = (fromEnum *** (`lookupEntityName` em)) $ case c of
    'T' -> (DirtT, "tree")
    _ -> (StoneT, "nothing")
