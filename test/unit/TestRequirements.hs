{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm requirements analysis tests
module TestRequirements where

import Data.Set qualified as S
import Data.Text (Text)
import Swarm.Language.Capability
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Pipeline
import Swarm.Language.Requirements.Analysis (requirements)
import Swarm.Language.Requirements.Type (ReqCtx, Requirements, capReqs, devReqs)
import Swarm.Language.Syntax.Constants (Const (Move))
import Swarm.Language.Syntax.Util (eraseS)
import Test.Tasty
import Test.Tasty.HUnit
import TestUtil (check)

testRequirements :: TestTree
testRequirements =
  testGroup
    "Requirements analysis"
    [ testGroup
        "Basic capabilities"
        [ testCase "solar panel" $ "noop" `requiresCap` CPower
        , testCase "move" $ "move" `requiresCap` CExecute Move
        , testCase "lambda" $ "\\x. x" `requiresCap` CLambda
        , testCase "inl" $ "inl 3" `requiresCap` CSum
        , testCase "cap from type" $ "inl () : rec t. Unit + t" `requiresCap` CRectype
        ]
    , testGroup
        "Scope"
        [ testCase "global var requirement does not apply to local var (#1914)" $
            checkReqCtx
              "def m = move end; def y = \\m. log (format m) end"
              (maybe False ((CExecute Move `S.notMember`) . capReqs) . Ctx.lookup "y")
        ]
    , testGroup
        "use"
        [ testCase "literal argument to use (#1301)" $
            "use \"key\"" `requiresDev` "key"
        ]
    ]

checkReqCtx :: Text -> (ReqCtx -> Bool) -> Assertion
checkReqCtx code expect = check code (expect . extractReqCtx)

checkRequirements :: Text -> (Requirements -> Bool) -> Assertion
checkRequirements code expect = check code (expect . requirements mempty mempty . eraseS)

requiresCap :: Text -> Capability -> Assertion
requiresCap code cap = checkRequirements code ((cap `S.member`) . capReqs)

requiresDev :: Text -> Text -> Assertion
requiresDev code dev = checkRequirements code ((dev `S.member`) . devReqs)
