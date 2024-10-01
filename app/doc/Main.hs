{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Data.Foldable qualified
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Options.Applicative
import Swarm.Doc.Gen (EdgeFilter (..), GenerateDocs (..), PageAddress (..), SheetType (..), generateDocs)
import Swarm.Doc.Keyword (EditorType (..))

cliParser :: Parser GenerateDocs
cliParser =
  subparser $
    mconcat
      [ command "recipes" (info (RecipeGraph <$> edgeFilter <**> helper) $ progDesc "Output graphviz dotfile of entity dependencies based on recipes")
      , command "editors" (info (EditorKeywords <$> editor <**> helper) $ progDesc "Output editor keywords")
      , command "keys" (info (pure SpecialKeyNames) $ progDesc "Output list of recognized special key names")
      , command "cheatsheet" (info (CheatSheet <$> address <*> cheatsheet <**> helper) $ progDesc "Output nice Wiki tables")
      , command "commands" (info (pure CommandsData <**> helper) $ progDesc "Output JSON data for commands matrix")
      , command "pedagogy" (info (pure TutorialCoverage) $ progDesc "Output tutorial coverage")
      ]
 where
  editor :: Parser (Maybe EditorType)
  editor =
    Data.Foldable.asum
      [ pure Nothing
      , Just VSCode <$ switch (long "code" <> help "Generate for the VS Code editor")
      , Just Emacs <$ switch (long "emacs" <> help "Generate for the Emacs editor")
      , Just Vim <$ switch (long "vim" <> help "Generate for the Vim editor")
      ]
  edgeFilter :: Parser EdgeFilter
  edgeFilter =
    Data.Foldable.asum
      [ pure NoFilter
      , FilterForward <$ switch (long "forward" <> help "Show only forward edges")
      , FilterNext <$ switch (long "next" <> help "Show only edges to next group")
      ]
  address :: Parser PageAddress
  address =
    let replace a b = T.unpack . T.replace a b . T.pack
        opt n =
          fmap (fromMaybe "") . optional $
            option
              str
              ( long n
                  <> metavar "ADDRESS"
                  <> help ("Set the address of " <> replace "-" " " n <> ". Default no link.")
              )
     in PageAddress <$> opt "entities-page" <*> opt "commands-page" <*> opt "capabilities-page" <*> opt "recipes-page"
  cheatsheet :: Parser SheetType
  cheatsheet =
    Data.Foldable.asum
      [ flag' Entities (long "entities" <> help "Generate entities page (uses data from entities.yaml)")
      , flag' Terrain (long "terrain" <> help "Generate terrain page (uses data from terrains.yaml)")
      , flag' Recipes (long "recipes" <> help "Generate recipes page (uses data from recipes.yaml)")
      , flag' Capabilities (long "capabilities" <> help "Generate capabilities page (uses entity map)")
      , flag' Commands (long "commands" <> help "Generate commands page (uses constInfo, constCaps and inferConst)")
      , flag' CommandMatrix (long "matrix" <> help "Generate commands matrix page")
      , flag' Scenario (long "scenario" <> help "Generate scenario schema page")
      ]

cliInfo :: ParserInfo GenerateDocs
cliInfo =
  info
    (cliParser <**> helper)
    ( header "Swarm docs"
        <> progDesc "Generate swarm documentation."
        <> fullDesc
    )

main :: IO ()
main = generateDocs =<< execParser cliInfo
