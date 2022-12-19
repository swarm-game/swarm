{-# LANGUAGE OverloadedStrings #-}

-- | Boolean expression unit tests
module TestBoolExpr where

import Data.BoolExpr qualified as BE
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set qualified as Set
import Swarm.Game.Scenario.Objective.Logic
import Swarm.Game.Scenario.Objective.Simplify qualified as Simplify
import Swarm.Game.Scenario.Objective.WinCheck qualified as WC
import Test.Tasty
import Test.Tasty.HUnit

testBoolExpr :: TestTree
testBoolExpr =
  testGroup
    "Boolean evaluation"
    [ testGroup
        "Expression simplification"
        [ testGroup
            "Return true if the expression can be simplified to False"
            [ testGroup
                "Effect of constant literals"
                [ testCase
                    "False input via single literal"
                    $ expectTrue BE.BFalse
                , testCase
                    "True input via composed literals"
                    $ expectFalse BE.BTrue
                , testCase
                    "False input via composed literals"
                    $ expectTrue
                    $ BE.BOr BE.BFalse BE.BFalse
                , testCase
                    "True input via composed literals"
                    $ expectFalse
                    $ BE.BOr BE.BFalse BE.BTrue
                , testCase
                    "Constant OR'd with False"
                    $ expectFalse
                    $ BE.BOr BE.BFalse (BE.BConst (BE.Positive "foo"))
                , testCase
                    "Constant OR'd with True"
                    $ expectFalse
                    $ BE.BOr (BE.BConst (BE.Positive "foo")) BE.BTrue
                , testCase
                    "Constant AND'd with False"
                    $ expectTrue
                    $ BE.BAnd BE.BFalse (BE.BConst (BE.Positive "foo"))
                , testCase
                    "Constant AND'd with True"
                    $ expectFalse
                    $ BE.BAnd (BE.BConst (BE.Positive "foo")) BE.BTrue
                , testCase
                    "Nested Constants AND'd with False within OR"
                    $ expectTrue
                    $ BE.BOr
                      (BE.BAnd BE.BFalse (BE.BConst (BE.Positive "foo")))
                      (BE.BAnd (BE.BConst (BE.Positive "bar")) BE.BFalse)
                , testCase
                    "Deeply nested Constants AND'd with False within OR with multiple negations"
                    $ expectTrue
                    $ BE.BOr
                      (BE.BAnd (BE.BNot BE.BTrue) (BE.BNot (BE.BNot (BE.BNot (BE.BConst (BE.Positive "foo"))))))
                      (BE.BAnd (BE.BConst (BE.Positive "bar")) (BE.BNot (BE.BNot BE.BFalse)))
                ]
            , testGroup
                "Effect of contradicting named constants"
                [ testCase
                    "via NOT operator"
                    $ expectTrue
                    $ BE.BAnd (BE.BNot (BE.BConst (BE.Positive "foo"))) (BE.BConst (BE.Positive "foo"))
                , testCase
                    "via signedness"
                    $ expectTrue
                    $ BE.BAnd (BE.BConst (BE.Positive "foo")) (BE.BConst (BE.Negative "foo"))
                ]
            ]
        ]
    , testGroup
        "Prerequisite expressions"
        [ testCase
            "A negated goal is completed"
            $ assertBool "Should have returned true"
            $ WC.isUnwinnablePrereq (Set.singleton "b") demoPrereqs
        , testCase
            "A non-negated goal is completed"
            $ assertBool "Should have returned false"
            $ not
            $ WC.isUnwinnablePrereq (Set.singleton "c") demoPrereqs
        ]
    ]
 where
  expectTrue, expectFalse :: BE.BoolExpr String -> Assertion
  expectTrue = assertBool "Should have returned true" . Simplify.cannotBeTrue
  expectFalse = assertBool "Should have returned false" . not . Simplify.cannotBeTrue

  demoPrereqs :: Prerequisite ObjectiveLabel
  demoPrereqs =
    And $
      Id "a"
        :| [Not (Id "b"), Id "c"]
