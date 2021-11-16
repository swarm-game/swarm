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
) where

import Control.Arrow ((***))
import Control.Lens hiding (from)
import Data.Array
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml as Y
import GHC.Int (Int64)
import Linear.V2
import Witch (from, into)

import Swarm.Game.Entity
import Swarm.Game.Robot (Robot)
import Swarm.Game.Terrain
import Swarm.Game.World
import Swarm.Language.Pipeline (ProcessedTerm)
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
      <*> mkWorldFun (v .: "world")
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

-- | A description of a world parsed from a YAML file.  The
--   'mkWorldFun' function is used to turn a 'WorldDescription' into a
--   'WorldFun'.
data WorldDescription = WorldDescription
  { defaultTerrain :: (TerrainType, Maybe Text)
  , palette :: WorldPalette
  , ul :: V2 Int64
  , area :: Text
  }

instance FromJSON WorldDescription where
  parseJSON = withObject "world description" $ \v ->
    WorldDescription
      <$> v .:? "default" .!= (BlankT, Nothing)
      <*> v .: "palette"
      <*> v .: "upperleft"
      <*> v .: "map"

newtype WorldPalette = WorldPalette
  {unPalette :: HashMap Text (TerrainType, Maybe Text)}

instance FromJSON WorldPalette where
  parseJSON = withObject "palette" $ fmap WorldPalette . mapM parseJSON

mkWorldFun :: Parser WorldDescription -> ParserE EntityMap (WorldFun Int Entity)
mkWorldFun pwd = E $ \em -> do
  wd <- pwd
  let toEntity :: Char -> Parser (Int, Maybe Entity)
      toEntity c = case HM.lookup (into @Text [c]) (unPalette (palette wd)) of
        Nothing -> fail $ "Char not in entity palette: " ++ [c]
        Just (t, mt) -> case mt of
          Nothing -> return (fromEnum t, Nothing)
          Just name -> case lookupEntityName name em of
            Nothing -> fail $ "Unknown entity name: " ++ from @Text name
            Just e -> return (fromEnum t, Just e)

      grid = map (into @String) . T.lines $ area wd

      rs = fromIntegral $ length grid
      cs = fromIntegral $ length (head grid)

      Coords (ulr, ulc) = locToCoords (ul wd)

  arr <-
    fmap (listArray ((ulr, ulc), (ulr + rs -1, ulc + cs -1)))
      . mapM toEntity
      . concat
      $ grid
  let defTerrain = (fromEnum *** (>>= (`lookupEntityName` em))) (defaultTerrain wd)
  return $ worldFunFromArray arr defTerrain
