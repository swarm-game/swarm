{-# LANGUAGE OverloadedStrings #-}

-- | Swarm unit tests
module Main where

import           Data.Text (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Swarm.Language.Pipeline
import           Swarm.Language.Pretty
import           Swarm.Language.Syntax

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parser, prettyConst]

parser :: TestTree
parser =
  testGroup
    "Language - pipeline"
    [ testCase "end semicolon #79" (valid "def a = 41 end def b = a + 1 end def c = b + 2 end")
    ]
  where
    valid = flip process ""
    process :: Text -> Text -> Assertion
    process code expect = case processTerm code of
      Left e | e == expect   -> pure ()
             | otherwise     -> error $ "Unexpected failure: " <> show e
      Right _ | expect == "" -> pure ()
              | otherwise    -> error "Unexpected success"

prettyConst :: TestTree
prettyConst =
  testGroup "Language - pretty"
    [ testCase "operators #8 - function application unchanged"
      (
        equalPretty "f say" $
        TApp (TVar "f") (TConst Say)
      )
    , testCase "operators #8 - double function application unchanged"
      (
        equalPretty "f () ()" $
        TApp (TApp (TVar "f") TUnit) TUnit
      )
    , testCase "operators #8 - embrace operator parameter"
      (
        equalPretty "f (==)" $
        TApp (TVar "f") (TConst Eq)
      )
    , testCase "operators #8 - simple infix operator"
      (
        equalPretty "1 == 2" $
        mkOp Eq (TInt 1) (TInt 2)
      )
    , testCase "operators #8 - infix operator with less fixing inner operator"
      (
        equalPretty "1 * (2 + 3)" $
        mkOp Mul (TInt 1) (mkOp Add (TInt 2) (TInt 3))
      )
    , testCase "operators #8 - infix operator with more fixing inner operator"
      (
        equalPretty "1 + 2 * 3" $
        mkOp Add (TInt 1) (mkOp Mul (TInt 2) (TInt 3))
      )
    , testCase "operators #8 - infix operator right associativity"
      (
        equalPretty "2 ^ 4 ^ 8" $
        mkOp Exp (TInt 2) (mkOp Exp (TInt 4) (TInt 8))
      )
    , testCase "operators #8 - infix operator right associativity not applied to left"
      (
        equalPretty "(2 ^ 4) ^ 8" $
        mkOp Exp (mkOp Exp (TInt 2) (TInt 4)) (TInt 8)
      )
    ]
  where
    equalPretty :: String -> Term -> Assertion
    equalPretty expected term = assertEqual "" expected . show $ ppr term
