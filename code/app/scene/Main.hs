{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Options.Applicative
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..))
import Swarm.Game.World.Render (FailureMode (..), OuputFormat (..), RenderOpts (..), doRenderCmd)

data CLI
  = RenderMap FilePath RenderOpts

cliParser :: Parser CLI
cliParser =
  RenderMap <$> strArgument (metavar "SCENARIO") <*> subOpts
 where
  sizeOpts =
    AreaDimensions
      <$> option auto (metavar "WIDTH" <> short 'w' <> long "width" <> help "width of source grid")
      <*> option auto (metavar "HEIGHT" <> short 'h' <> long "height" <> help "height of source grid")

  subOpts =
    RenderOpts
      <$> seed
      <*> flag ConsoleText PngImage (long "png" <> help "Render to PNG")
      <*> option str (long "dest" <> short 'd' <> value "output.png" <> help "Output filepath")
      <*> optional sizeOpts
      <*> flag Terminate RenderBlankImage (long "fail-blank" <> short 'b' <> help "Render blank image upon failure")

  seed :: Parser (Maybe Int)
  seed = optional $ option auto (long "seed" <> short 's' <> metavar "INT" <> help "Seed to use for world generation")

cliInfo :: ParserInfo CLI
cliInfo =
  info
    (cliParser <**> helper)
    ( header "Swarm scene"
        <> progDesc "Render a scenario world map."
        <> fullDesc
    )

main :: IO ()
main = do
  cli <- execParser cliInfo
  case cli of
    RenderMap mapPath opts -> doRenderCmd opts mapPath
