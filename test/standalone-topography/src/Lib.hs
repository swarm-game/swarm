{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Lib (compareToReferenceImage) where

import Codec.Picture
import Control.Arrow (left)
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Utils (forceEither)
import Data.Yaml (prettyPrintParseException)
import GHC.Generics (Generic)
import Paths_swarm (getDataDir)
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (
  Parentage (Root),
 )
import Swarm.Game.Scenario.Topography.Rasterize
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Util.Yaml
import System.FilePath
import Test.Tasty.HUnit (Assertion, assertEqual)

newtype CustomCell = CustomCell Bool
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance FromJSONE e CustomCell

parseStructures :: IO (PStructure (Maybe CustomCell))
parseStructures = do
  dataDir <- getDataDir
  eitherResult <-
    decodeFileEitherE () $
      dataDir </> "test/standalone-topography/demo-structures.yaml"
  return $ forceEither $ left prettyPrintParseException eitherResult

getDisplayColor :: Maybe CustomCell -> PixelRGBA8
getDisplayColor = maybe transparent mkPixelColor
 where
  mkPixelColor (CustomCell b) = case b of
    False -> PixelRGBA8 0 0 0 255
    True -> PixelRGBA8 255 255 255 255

  transparent = PixelRGBA8 0 0 0 0

compareToReferenceImage :: Assertion
compareToReferenceImage = do
  parentStruct <- parseStructures
  let MergedStructure overlayArea _ _ = forceEither $ mergeStructures mempty Root parentStruct
  let encodedImgBytestring = encodePng $ makeImage getDisplayColor $ gridContent overlayArea

  dataDir <- getDataDir
  let referenceFilepath = dataDir </> "test/standalone-topography/reference.png"
  decodedImg <- LBS.readFile referenceFilepath
  assertEqual "Generated image must equal reference image!" decodedImg encodedImgBytestring
