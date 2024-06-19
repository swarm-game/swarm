{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Data.Foldable qualified
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Options.Applicative
import Swarm.Doc.Gen (GenerateDocs (..), PageAddress (..), SheetType (..), generateDocs)
import Swarm.Doc.Keyword (EditorType (..))

cliParser :: Parser GenerateDocs
cliParser =
  subparser $
    mconcat
      [ command "recipes" (info (pure RecipeGraph) $ progDesc "Output graphviz dotfile of entity dependencies based on recipes")
      , command "editors" (info (EditorKeywords <$> editor <**> helper) $ progDesc "Output editor keywords")
      , command "keys" (info (pure SpecialKeyNames) $ progDesc "Output list of recognized special key names")
      , command "cheatsheet" (info (CheatSheet <$> address <*> cheatsheet <**> helper) $ progDesc "Output nice Wiki tables")
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
  cheatsheet :: Parser (Maybe SheetType)
  cheatsheet =
    Data.Foldable.asum
      [ pure Nothing
      , Just Entities <$ switch (long "entities" <> help "Generate entities page (uses data from entities.yaml)")
      , Just Terrain <$ switch (long "terrain" <> help "Generate terrain page (uses data from terrains.yaml)")
      , Just Recipes <$ switch (long "recipes" <> help "Generate recipes page (uses data from recipes.yaml)")
      , Just Capabilities <$ switch (long "capabilities" <> help "Generate capabilities page (uses entity map)")
      , Just Commands <$ switch (long "commands" <> help "Generate commands page (uses constInfo, constCaps and inferConst)")
      , Just CommandMatrix <$ switch (long "matrix" <> help "Generate commands matrix page")
      , Just Scenario <$ switch (long "scenario" <> help "Generate scenario schema page")
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
