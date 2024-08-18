{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Data.Proxy
import Data.Typeable (Typeable)
import Lib (compareToReferenceImage)
import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Options

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
  defaultMainWithIngredients ingredients $ askOption $ \(UpdateGoldenTests shouldRefreshTests) ->
    let doTest stem =
          testCase (unwords ["Image equality:", stem]) $
            compareToReferenceImage shouldRefreshTests stem

        mkGroup title members =
          testGroup title $
            map
              doTest
              members
     in testGroup
          "Test structure assembly"
          [ mkGroup
              "Black and white"
              [ "circle-and-crosses"
              , "checkerboard"
              ]
          , mkGroup
              "Color"
              [ "rainbow"
              ]
          ]
 where
  ingredients = includingOptions [Option (Proxy :: Proxy UpdateGoldenTests)] : defaultIngredients
