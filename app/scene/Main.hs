{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Options.Applicative
import Swarm.Failure (simpleErrorHandle)
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..))
import Swarm.Game.World.Render (FailureMode (..), OuputFormat (..), RenderComputationContext (..), RenderOpts (..), doRenderCmd)
import Swarm.Render.Structures

data CLIToplevel
  = CLIToplevel FilePath CLI

data CLI
  = RenderMap RenderOpts
  | RenderStructures RenderOpts

cliParser :: Parser CLIToplevel
cliParser =
  CLIToplevel
    <$> strArgument (metavar "SCENARIO")
    <*> ( subparser
            ( mconcat
                [ command "scene" (info (RenderMap <$> subOpts <**> helper) (progDesc "Run the Swarm game (default)"))
                , command "structures" (info (RenderStructures <$> subOpts <**> helper) (progDesc "Format a file"))
                ]
            )
            <|> (RenderMap <$> subOpts)
        )
 where
  sizeOpts =
    AreaDimensions
      <$> option auto (metavar "WIDTH" <> short 'w' <> long "width" <> help "width of source grid")
      <*> option auto (metavar "HEIGHT" <> short 'h' <> long "height" <> help "height of source grid")

  renderComputationOpts =
    RenderComputationContext
      <$> seed
      <*> optional sizeOpts

  subOpts =
    RenderOpts
      <$> renderComputationOpts
      <*> flag ConsoleText PngImage (long "png" <> help "Render to PNG")
      <*> option str (long "dest" <> short 'd' <> value "output.png" <> help "Output filepath")
      <*> flag Terminate RenderBlankImage (long "fail-blank" <> short 'b' <> help "Render blank image upon failure")

  seed :: Parser (Maybe Int)
  seed = optional $ option auto (long "seed" <> short 's' <> metavar "INT" <> help "Seed to use for world generation")

cliInfo :: ParserInfo CLIToplevel
cliInfo =
  info
    (cliParser <**> helper)
    ( header "Swarm scene"
        <> progDesc "Render a scenario world map."
        <> fullDesc
    )

main :: IO ()
main = do
  CLIToplevel mapPath cli <- execParser cliInfo
  case cli of
    RenderMap opts -> doRenderCmd opts mapPath
    RenderStructures opts ->
      simpleErrorHandle $
        doRenderStructures mapPath $
          outputFilepath opts
