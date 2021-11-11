{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Lens hiding (from)
import Data.Array
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Yaml as Y
import Linear.V2

import Swarm.Game.Entity
import Swarm.Game.Robot (Robot, baseRobot, robotLocation)
import Swarm.Game.Terrain
import Swarm.Game.World
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Util.Yaml

-- | A 'Challenge' contains all the information to describe a
--   challenge.
data Challenge = Challenge
  { _challengeName :: Text
  , _challengeSeed :: Maybe Int
  , _challengeWorld :: WorldFun Int Entity
  , _challengeRobots :: [Robot]
  , _challengeWin :: ProcessedTerm
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''Challenge

instance FromJSONE EntityMap Challenge where
  parseJSONE = withObjectE "challenge" $ \v ->
    Challenge
      <$> liftE (v .: "name")
      <*> liftE (v .:? "seed")
      <*> pure (const (fromEnum StoneT, Nothing)) -- XXX
      <*> v ..: "robots"
      <*> liftE (v .: "win")

-- | the name of the challenge.
challengeName :: Lens' Challenge Text

-- | The seed used for the random number generator.  If @Nothing@, use
--   a random seed.
challengeSeed :: Lens' Challenge (Maybe Int)

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
    , _challengeSeed = Just 0
    , _challengeWorld = worldFunFromArray arr (fromEnum StoneT, Nothing)
    , _challengeRobots =
        [baseRobot startDevices & robotLocation .~ V2 0 0]
    , _challengeWin =
        [tmQ| loc <- getRobotLoc "base";
              return (loc == inr (4,4))
            |]
    }
 where
  startDevices = mapMaybe (`lookupEntityName` em) ["treads", "dictionary", "scanner"]

  arr =
    listArray ((-4, 0), (0, 4)) . map toEntity . concat $
      [ "  T  "
      , " T   "
      , "     "
      , "   T "
      , "T    "
      ]
  toEntity c = (fromEnum *** (`lookupEntityName` em)) $ case c of
    'T' -> (DirtT, "tree")
    _ -> (StoneT, "nothing")
