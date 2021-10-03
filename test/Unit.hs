{-# LANGUAGE OverloadedStrings #-}

-- | Swarm unit tests
module Main where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Swarm.Language.Pipeline
import Swarm.Language.Pretty
import Swarm.Language.Syntax hiding (mkOp)

import Swarm.Game.Display (defaultRobotDisplay)
import qualified Swarm.Game.Entity as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parser, prettyConst, inventory]

parser :: TestTree
parser =
  testGroup
    "Language - pipeline"
    [ testCase "end semicolon #79" (valid "def a = 41 end def b = a + 1 end def c = b + 2 end")
    , testCase "located type error" (process "def a =\n 42 + \"oops\"\nend" "2: Can't unify int and string")
    , testCase
        "quantification #148 - implicit"
        (valid "def id : a -> a = \\x. x end; id move")
    , testCase
        "quantification #148 - explicit"
        (valid "def id : forall a. a -> a = \\x. x end; id move")
    , testCase
        "quantification #148 - explicit with free tyvars"
        ( process
            "def id : forall a. b -> b = \\x. x end; id move"
            ( T.unlines
                [ "1:27:"
                , "  |"
                , "1 | def id : forall a. b -> b = \\x. x end; id move"
                , "  |                           ^"
                , "  Type contains free variable(s): b"
                , "  Try adding them to the 'forall'."
                , ""
                ]
            )
        )
    ]
 where
  valid = flip process ""
  process :: Text -> Text -> Assertion
  process code expect = case processTerm code of
    Left e
      | e == expect -> pure ()
      | otherwise -> error $ "Unexpected failure: " <> show e
    Right _
      | expect == "" -> pure ()
      | otherwise -> error "Unexpected success"

prettyConst :: TestTree
prettyConst =
  testGroup
    "Language - pretty"
    [ testCase
        "operators #8 - function application unchanged"
        ( equalPretty "f say" $
            TApp (TVar "f") (TConst Say)
        )
    , testCase
        "operators #8 - double function application unchanged"
        ( equalPretty "f () ()" $
            TApp (TApp (TVar "f") TUnit) TUnit
        )
    , testCase
        "operators #8 - embrace operator parameter"
        ( equalPretty "f (==)" $
            TApp (TVar "f") (TConst Eq)
        )
    , testCase
        "operators #8 - unary negation"
        ( equalPretty "-3" $
            TApp (TConst Neg) (TInt 3)
        )
    , testCase
        "operators #8 - double unary negation"
        ( equalPretty "-(-1)" $
            TApp (TConst Neg) $ TApp (TConst Neg) (TInt 1)
        )
    , testCase
        "operators #8 - unary negation with strongly fixing binary operator"
        ( equalPretty "-1 ^ (-2)" $
            TApp (TConst Neg) $ mkOp' Exp (TInt 1) $ TApp (TConst Neg) (TInt 2)
        )
    , testCase
        "operators #8 - unary negation with weakly fixing binary operator"
        ( equalPretty "-(1 + -2)" $
            TApp (TConst Neg) $ mkOp' Add (TInt 1) $ TApp (TConst Neg) (TInt 2)
        )
    , testCase
        "operators #8 - simple infix operator"
        ( equalPretty "1 == 2" $
            mkOp' Eq (TInt 1) (TInt 2)
        )
    , testCase
        "operators #8 - infix operator with less fixing inner operator"
        ( equalPretty "1 * (2 + 3)" $
            mkOp' Mul (TInt 1) (mkOp' Add (TInt 2) (TInt 3))
        )
    , testCase
        "operators #8 - infix operator with more fixing inner operator"
        ( equalPretty "1 + 2 * 3" $
            mkOp' Add (TInt 1) (mkOp' Mul (TInt 2) (TInt 3))
        )
    , testCase
        "operators #8 - infix operator right associativity"
        ( equalPretty "2 ^ 4 ^ 8" $
            mkOp' Exp (TInt 2) (mkOp' Exp (TInt 4) (TInt 8))
        )
    , testCase
        "operators #8 - infix operator right associativity not applied to left"
        ( equalPretty "(2 ^ 4) ^ 8" $
            mkOp' Exp (mkOp' Exp (TInt 2) (TInt 4)) (TInt 8)
        )
    ]
 where
  equalPretty :: String -> Term -> Assertion
  equalPretty expected term = assertEqual "" expected . show $ ppr term

inventory :: TestTree
inventory =
  testGroup
    "Inventory"
    [ testCase "byName case insensitive insert delete" $
        let e = E.mkEntity defaultRobotDisplay "WaCkYcAsE" [] []
         in assertEqual "" (E.empty & E.insert e & E.delete e & E.lookupByName (e ^. E.entityName)) []
    ]
