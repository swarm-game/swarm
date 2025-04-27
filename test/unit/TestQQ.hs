{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Unit tests for quasiquoting
module TestQQ where

import Swarm.Language.Parser.QQ
import Swarm.Language.Syntax
import Swarm.Pretty
import Test.Tasty
import Test.Tasty.HUnit

testQQ :: TestTree
testQQ =
  testGroup
    "quasiquoters"
    [ testCase "basic untyped AST quasiquote" $
        assertEqual
          "numeric literal"
          [astQ| 3 |]
          (Syntax' (SrcLoc 1 2) (TInt 3) mempty ())
    , testCase "antiquoting for untyped AST quasiquotes" $
        let t = [astQ| 3 + 6 |]
         in assertEqual
              "foo"
              (prettyText [astQ| def x : Int -> Cmd Bool = \x. pure (x == $syn:t) end |])
              (prettyText [astQ| def x : Int -> Cmd Bool = \x. pure (x == 3 + 6) end |])
    ]

-- Note, they are not equal as ASTs because the SrcLoc
-- annotations differ.
