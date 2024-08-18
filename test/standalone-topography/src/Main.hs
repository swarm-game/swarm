{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit ( testCase )
import Data.Typeable (Typeable)
import Data.Proxy

import Lib ( compareToReferenceImage )

newtype UpdateGoldenTests = UpdateGoldenTests Bool
  deriving (Eq, Ord, Typeable)

instance IsOption UpdateGoldenTests where
  parseValue = fmap UpdateGoldenTests . safeRead
  defaultValue = UpdateGoldenTests False
  optionName = return "refresh"
  optionHelp = return "Should overwrite the golden test images"
  optionCLParser = mkFlagCLParser mempty (UpdateGoldenTests True)

main :: IO ()
main = do
  defaultMainWithIngredients ingreds $ askOption $ \(UpdateGoldenTests shouldRefreshTests) -> let
      doTest stem =
        testCase (unwords ["Image equality:", stem]) $
          compareToReferenceImage shouldRefreshTests stem

      mkGroup title members =
        testGroup title $
          map
            doTest
            members
    in
    testGroup
      "Test structure assembly"
      [ mkGroup
          "Black and white"
          [ "circle-and-crosses"
          , "checkerboard"
          ]
      , mkGroup
          "Color"
          [ "rainbow"
          , "flip-duplicate"
          ]
      ]
 where
  ingreds = includingOptions [Option (Proxy :: Proxy UpdateGoldenTests)] : defaultIngredients

