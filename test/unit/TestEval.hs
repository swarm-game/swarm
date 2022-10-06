{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Swarm unit tests
module TestEval where

import Control.Lens ((^.), _3)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.State
import Swarm.Game.Value
import Swarm.Util (quote)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestUtil
import Witch (from)

testEval :: GameState -> TestTree
testEval g =
  testGroup
    "Language - evaluation"
    [ testGroup
        "arithmetic"
        [ testProperty
            "addition"
            (\a b -> binOp a "+" b `evaluatesToP` VInt (a + b))
        , testProperty
            "subtraction"
            (\a b -> binOp a "-" b `evaluatesToP` VInt (a - b))
        , testProperty
            "multiplication"
            (\a b -> binOp a "*" b `evaluatesToP` VInt (a * b))
        , testProperty
            "division"
            (\a (NonZero b) -> binOp a "/" b `evaluatesToP` VInt (a `div` b))
        , testProperty
            "exponentiation"
            (\a (NonNegative b) -> binOp a "^" b `evaluatesToP` VInt (a ^ (b :: Integer)))
        ]
    , testGroup
        "int comparison"
        [ testProperty
            "=="
            (\a b -> binOp a "==" b `evaluatesToP` VBool ((a :: Integer) == b))
        , testProperty
            "<"
            (\a b -> binOp a "<" b `evaluatesToP` VBool ((a :: Integer) < b))
        , testProperty
            "<="
            (\a b -> binOp a "<=" b `evaluatesToP` VBool ((a :: Integer) <= b))
        , testProperty
            ">"
            (\a b -> binOp a ">" b `evaluatesToP` VBool ((a :: Integer) > b))
        , testProperty
            ">="
            (\a b -> binOp a ">=" b `evaluatesToP` VBool ((a :: Integer) >= b))
        , testProperty
            "!="
            (\a b -> binOp a "!=" b `evaluatesToP` VBool ((a :: Integer) /= b))
        ]
    , testGroup
        "pair comparison"
        [ testProperty
            "=="
            (\a b -> binOp a "==" b `evaluatesToP` VBool ((a :: (Integer, Integer)) == b))
        , testProperty
            "<"
            (\a b -> binOp a "<" b `evaluatesToP` VBool ((a :: (Integer, Integer)) < b))
        , testProperty
            "<="
            (\a b -> binOp a "<=" b `evaluatesToP` VBool ((a :: (Integer, Integer)) <= b))
        , testProperty
            ">"
            (\a b -> binOp a ">" b `evaluatesToP` VBool ((a :: (Integer, Integer)) > b))
        , testProperty
            ">="
            (\a b -> binOp a ">=" b `evaluatesToP` VBool ((a :: (Integer, Integer)) >= b))
        , testProperty
            "!="
            (\a b -> binOp a "!=" b `evaluatesToP` VBool ((a :: (Integer, Integer)) /= b))
        ]
    , testGroup
        "boolean operators"
        [ testCase
            "and"
            ("true && false" `evaluatesTo` VBool False)
        , testCase
            "or"
            ("true || false" `evaluatesTo` VBool True)
        ]
    , testGroup
        "sum types #224"
        [ testCase
            "inl"
            ("inl 3" `evaluatesTo` VInj False (VInt 3))
        , testCase
            "inr"
            ("inr \"hi\"" `evaluatesTo` VInj True (VText "hi"))
        , testCase
            "inl a < inl b"
            ("inl 3 < inl 4" `evaluatesTo` VBool True)
        , testCase
            "inl b < inl a"
            ("inl 4 < inl 3" `evaluatesTo` VBool False)
        , testCase
            "inl < inr"
            ("inl 3 < inr true" `evaluatesTo` VBool True)
        , testCase
            "inl 4 < inr 3"
            ("inl 4 < inr 3" `evaluatesTo` VBool True)
        , testCase
            "inr < inl"
            ("inr 3 < inl true" `evaluatesTo` VBool False)
        , testCase
            "inr 3 < inl 4"
            ("inr 3 < inl 4" `evaluatesTo` VBool False)
        , testCase
            "inr a < inr b"
            ("inr 3 < inr 4" `evaluatesTo` VBool True)
        , testCase
            "inr b < inr a"
            ("inr 4 < inr 3" `evaluatesTo` VBool False)
        , testCase
            "case inl"
            ("case (inl 2) (\\x. x + 1) (\\y. y * 17)" `evaluatesTo` VInt 3)
        , testCase
            "case inr"
            ("case (inr 2) (\\x. x + 1) (\\y. y * 17)" `evaluatesTo` VInt 34)
        , testCase
            "nested 1"
            ("(\\x : int + bool + text. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inl 3)" `evaluatesTo` VInt 1)
        , testCase
            "nested 2"
            ("(\\x : int + bool + text. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inl false))" `evaluatesTo` VInt 2)
        , testCase
            "nested 2"
            ("(\\x : int + bool + text. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inr \"hi\"))" `evaluatesTo` VInt 3)
        ]
    , testGroup
        "operator evaluation"
        [ testCase
            "application operator #239"
            ("fst $ snd $ (1,2,3)" `evaluatesTo` VInt 2)
        ]
    , testGroup
        "recursive bindings"
        [ testCase
            "factorial"
            ("let fac = \\n. if (n==0) {1} {n * fac (n-1)} in fac 15" `evaluatesTo` VInt 1307674368000)
        , testCase
            "loop detected"
            ("let x = x in x" `throwsError` ("loop detected" `T.isInfixOf`))
        ]
    , testGroup
        "delay"
        [ testCase
            "force / delay"
            ("force {10}" `evaluatesTo` VInt 10)
        , testCase
            "force x2 / delay x2"
            ("force (force { {10} })" `evaluatesTo` VInt 10)
        , testCase
            "if is lazy"
            ("if true {1} {1/0}" `evaluatesTo` VInt 1)
        , testCase
            "function with if is not lazy"
            ( "let f = \\x. \\y. if true {x} {y} in f 1 (1/0)"
                `throwsError` ("by zero" `T.isInfixOf`)
            )
        , testCase
            "memoization baseline"
            ( "def fac = \\n. if (n==0) {1} {n * fac (n-1)} end; def f10 = fac 10 end; let x = f10 in noop"
                `evaluatesToInAtMost` (VUnit, 535)
            )
        , testCase
            "memoization"
            ( "def fac = \\n. if (n==0) {1} {n * fac (n-1)} end; def f10 = fac 10 end; let x = f10 in let y = f10 in noop"
                `evaluatesToInAtMost` (VUnit, 540)
            )
        ]
    , testGroup
        "conditions"
        [ testCase
            "if true"
            ("if true {1} {2}" `evaluatesTo` VInt 1)
        , testCase
            "if false"
            ("if false {1} {2}" `evaluatesTo` VInt 2)
        , testCase
            "if (complex condition)"
            ("if (let x = 3 + 7 in not (x < 2^5)) {1} {2}" `evaluatesTo` VInt 2)
        ]
    , testGroup
        "exceptions"
        [ testCase
            "fail"
            ("fail \"foo\"" `throwsError` ("foo" `T.isInfixOf`))
        , testCase
            "try / no exception 1"
            ("try {return 1} {return 2}" `evaluatesTo` VInt 1)
        , testCase
            "try / no exception 2"
            ("try {return 1} {let x = x in x}" `evaluatesTo` VInt 1)
        , testCase
            "try / fail"
            ("try {fail \"foo\"} {return 3}" `evaluatesTo` VInt 3)
        , testCase
            "try / fail / fail"
            ("try {fail \"foo\"} {fail \"bar\"}" `throwsError` ("bar" `T.isInfixOf`))
        , testCase
            "try / div by 0"
            ("try {return (1/0)} {return 3}" `evaluatesTo` VInt 3)
        ]
    , testGroup
        "text"
        [ testCase
            "format int"
            ("format 1" `evaluatesTo` VText "1")
        , testCase
            "format sum"
            ("format (inl 1)" `evaluatesTo` VText "inl 1")
        , testCase
            "format function"
            ("format (\\x. x + 1)" `evaluatesTo` VText "\\x. x + 1")
        , testCase
            "concat"
            ("\"x = \" ++ format (2+3) ++ \"!\"" `evaluatesTo` VText "x = 5!")
        , testProperty
            "number of characters"
            ( \s ->
                ("chars " <> tquote s) `evaluatesToP` VInt (fromIntegral $ length s)
            )
        , testProperty
            "split undo concatenation"
            ( \s1 s2 ->
                -- \s1.\s2. (s1,s2) == split (chars s1) (s1 ++ s2)
                let (t1, t2) = (tquote s1, tquote s2)
                 in T.concat ["(", t1, ",", t2, ") == split (chars ", t1, ") (", t1, " ++ ", t2, ")"]
                      `evaluatesToP` VBool True
            )
        ]
    ]
 where
  tquote :: String -> Text
  tquote = T.pack . show
  throwsError :: Text -> (Text -> Bool) -> Assertion
  throwsError tm p = do
    result <- evaluate tm
    case result of
      Right _ -> assertFailure "Unexpected success"
      Left err ->
        p err
          @? "Expected predicate did not hold on error message "
          ++ from @Text @String err

  evaluatesTo :: Text -> Value -> Assertion
  evaluatesTo tm val = do
    result <- evaluate tm
    assertEqual "" (Right val) (fst <$> result)

  evaluatesToP :: Text -> Value -> Property
  evaluatesToP tm val = ioProperty $ do
    result <- evaluate tm
    return $ Right val === (fst <$> result)

  evaluatesToInAtMost :: Text -> (Value, Int) -> Assertion
  evaluatesToInAtMost tm (val, maxSteps) = do
    result <- evaluate tm
    case result of
      Left err -> assertFailure ("Evaluation failed: " ++ from @Text @String err)
      Right (v, steps) -> do
        assertEqual "" val v
        assertBool ("Took more than " ++ show maxSteps ++ " steps!") (steps <= maxSteps)

  evaluate :: Text -> IO (Either Text (Value, Int))
  evaluate = fmap (^. _3) . eval g

binOp :: Show a => a -> String -> a -> Text
binOp a op b = from @String (p (show a) ++ op ++ p (show b))
 where
  p x = "(" ++ x ++ ")"
