{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm command implementation unit tests
module TestCommand where

import Data.Set qualified as S
import Data.Text (Text)
import Graphics.Vty.Input.Events qualified as V
import Swarm.Game.Location
import Swarm.Language.Key
import Swarm.Language.Parse (runParser)
import Swarm.Language.Syntax
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Witch

testCommands :: TestTree
testCommands =
  testGroup
    "Command implementations"
    [ testGroup
        "Directions"
        [ testGroup
            "Relative direction"
            [ testCase
                "West to East"
                $ assertEqual "Incorrect relative dir" (relativeTo DWest DEast) DBack
            , testCase
                "South to South"
                $ assertEqual "Incorrect relative dir" (relativeTo DSouth DSouth) DForward
            , testCase
                "South to West"
                $ assertEqual "Incorrect relative dir" (DSouth `relativeTo` DWest) DLeft
            , testCase
                "West to South"
                $ assertEqual "Incorrect relative dir" (DWest `relativeTo` DSouth) DRight
            ]
        ]
    , testGroup
        "Keys"
        [ testGroup
            "Parsing"
            ( let parseKeyTest input mods k =
                    assertEqual "" (runParser parseKeyCombo input) (Right (mkKeyCombo mods k))
               in [ testCase "parse x" $ parseKeyTest "x" [] (V.KChar 'x')
                  , testCase "parse X" $ parseKeyTest "X" [] (V.KChar 'X')
                  , testCase "parse C" $ parseKeyTest "C" [] (V.KChar 'C')
                  , testCase "parse F" $ parseKeyTest "F" [] (V.KChar 'F')
                  , testCase "parse F3" $ parseKeyTest "F3" [] (V.KFun 3)
                  , testCase "parse F12" $ parseKeyTest "F12" [] (V.KFun 12)
                  , testCase "parse Down" $ parseKeyTest "Down" [] V.KDown
                  , testCase "parse DownLeft" $ parseKeyTest "DownLeft" [] V.KDownLeft
                  , testCase "parse C-x" $ parseKeyTest "C-x" [V.MCtrl] (V.KChar 'x')
                  , testCase "parse S-x" $ parseKeyTest "S-x" [V.MShift] (V.KChar 'x')
                  , testCase "parse A-x" $ parseKeyTest "A-x" [V.MAlt] (V.KChar 'x')
                  , testCase "parse M-x" $ parseKeyTest "M-x" [V.MMeta] (V.KChar 'x')
                  , testCase "parse M-C-x" $ parseKeyTest "M-C-x" [V.MCtrl, V.MMeta] (V.KChar 'x')
                  , testCase "parse C-M-x" $ parseKeyTest "C-M-x" [V.MCtrl, V.MMeta] (V.KChar 'x')
                  ]
            )
        , testGroup
            "Pretty-printing"
            [ testProperty
                "(parse . pretty) key round trip"
                prop_parse_pretty_key
            ]
        ]
    ]

instance QC.Arbitrary KeyCombo where
  arbitrary = mkKeyCombo <$> arbitraryModifiers <*> arbitraryKey

arbitraryKey :: QC.Gen V.Key
arbitraryKey =
  QC.frequency $
    [ (10, V.KChar <$> QC.arbitrary)
    , (3, V.KFun . QC.getPositive <$> QC.arbitrary)
    ]
      ++ map ((1,) . pure . read . ('K' :) . from @Text) (S.toList specialKeyNames)

arbitraryModifiers :: QC.Gen [V.Modifier]
arbitraryModifiers = QC.sublistOf [V.MAlt, V.MCtrl, V.MMeta, V.MShift]

prop_parse_pretty_key :: KeyCombo -> Bool
prop_parse_pretty_key kc =
  runParser parseKeyCombo (prettyKeyCombo kc) == Right kc
