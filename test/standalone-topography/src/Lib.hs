{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Lib (compareToReferenceImage) where

import Codec.Picture
import Control.Arrow (left)
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Utils (forceEither)
import Data.Yaml (prettyPrintParseException)
import Paths_swarm (getDataDir)
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (
  Parentage (Root),
 )
import Swarm.Game.Scenario.Topography.Rasterize
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.World.Render
import Swarm.Util.Yaml
import System.FilePath
import Test.Tasty.HUnit (Assertion, assertEqual)

parseStructures ::
  FilePath ->
  FilePath ->
  IO (PStructure (Maybe ColorableCell))
parseStructures dataDir baseFilename = do
  eitherResult <-
    decodeFileEitherE () $
      dataDir </> "test/standalone-topography" </> baseFilename
  return $ forceEither $ left prettyPrintParseException eitherResult

compareToReferenceImage ::
  Bool -- ^ set this to update the golden tests
  -> FilePath
  -> Assertion
compareToReferenceImage refreshReferenceImage fileStem = do
  dataDir <- getDataDir
  parentStruct <- parseStructures dataDir $ fileStem <.> "yaml"
  let MergedStructure overlayArea _ _ = forceEither $ mergeStructures mempty Root parentStruct
      encodedImgBytestring = encodePng $ makeImage $ gridContent overlayArea
      referenceFilepath = dataDir </> "test/standalone-topography" </> fileStem <.> "png"
  if refreshReferenceImage
    then LBS.writeFile referenceFilepath encodedImgBytestring
    else do
      decodedImg <- LBS.readFile referenceFilepath
      assertEqual "Generated image must equal reference image!" decodedImg encodedImgBytestring
