{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestPretty where

import Data.Fix (Fix (..))
import Swarm.Pretty
import Swarm.Language.Syntax hiding (mkOp)
import Swarm.Language.Types
import Test.Tasty
import Test.Tasty.HUnit
import Witch (into)

testPrettyConst :: TestTree
testPrettyConst =
  testGroup
    "Language - pretty"
    [ testCase
        "operators #8 - function application unchanged"
        ( equalPrettyLine "f say" $
            TApp (TVar "f") (TConst Say)
        )
    , testCase
        "operators #8 - double function application unchanged"
        ( equalPrettyLine "f () ()" $
            TApp (TApp (TVar "f") TUnit) TUnit
        )
    , testCase
        "operators #8 - embrace operator parameter"
        ( equalPrettyLine "f (==)" $
            TApp (TVar "f") (TConst Eq)
        )
    , testCase
        "operators #8 - unary negation"
        ( equalPrettyLine "-3" $
            TApp (TConst Neg) (TInt 3)
        )
    , testCase
        "operators #8 - double unary negation"
        ( equalPrettyLine "-(-1)" $
            TApp (TConst Neg) $
              TApp (TConst Neg) (TInt 1)
        )
    , testCase
        "operators #8 - unary negation with strongly fixing binary operator"
        ( equalPrettyLine "-1 ^ (-2)"
            . TApp (TConst Neg)
            . mkOp' Exp (TInt 1)
            $ TApp (TConst Neg) (TInt 2)
        )
    , testCase
        "operators #8 - unary negation with weakly fixing binary operator"
        ( equalPrettyLine "-(1 + -2)"
            . TApp (TConst Neg)
            . mkOp' Add (TInt 1)
            $ TApp (TConst Neg) (TInt 2)
        )
    , testCase
        "operators #8 - simple infix operator"
        ( equalPrettyLine "1 == 2" $
            mkOp' Eq (TInt 1) (TInt 2)
        )
    , testCase
        "operators #8 - infix operator with less fixing inner operator"
        ( equalPrettyLine "1 * (2 + 3)" $
            mkOp' Mul (TInt 1) (mkOp' Add (TInt 2) (TInt 3))
        )
    , testCase
        "operators #8 - infix operator with more fixing inner operator"
        ( equalPrettyLine "1 + 2 * 3" $
            mkOp' Add (TInt 1) (mkOp' Mul (TInt 2) (TInt 3))
        )
    , testCase
        "operators #8 - infix operator right associativity"
        ( equalPrettyLine "2 ^ 4 ^ 8" $
            mkOp' Exp (TInt 2) (mkOp' Exp (TInt 4) (TInt 8))
        )
    , testCase
        "operators #8 - infix operator right associativity not applied to left"
        ( equalPrettyLine "(2 ^ 4) ^ 8" $
            mkOp' Exp (mkOp' Exp (TInt 2) (TInt 4)) (TInt 8)
        )
    , testCase
        "pairs #225 - nested pairs are printed right-associative"
        ( equalPrettyLine "(1, 2, 3)" $
            TPair (TInt 1) (TPair (TInt 2) (TInt 3))
        )
    , testCase
        "type ascription"
        ( equalPrettyLine "1 : Int" $
            TAnnotate (TInt 1) (Forall [] TyInt)
        )
    , testCase
        "lambda precedence (#1468)"
        ( equalPrettyLine "\\m. case m (\\x. x + 1) (\\y. y * 2)" $
            TLam
              "m"
              Nothing
              ( TConst Case
                  :$: STerm (TVar "m")
                  :$: STerm (TLam "x" Nothing (mkOp' Add (TVar "x") (TInt 1)))
                  :$: STerm (TLam "y" Nothing (mkOp' Mul (TVar "y") (TInt 2)))
              )
        )
    , testGroup
        "types"
        [ testCase
            "Void type"
            ( equalPrettyLine "Void" TyVoid
            )
        , testCase
            "Unit type"
            ( equalPrettyLine "Unit" TyUnit
            )
        , testCase
            "Function type"
            ( equalPrettyLine "Int -> Cmd Unit" $ TyInt :->: TyCmd TyUnit
            )
        , testCase
            "Cmd type"
            ( equalPrettyLine "Cmd (Int -> Int)" $ TyCmd (TyInt :->: TyInt)
            )
        , testCase
            "Product type"
            ( equalPrettyLine "Int * Int" $ TyInt :*: TyInt
            )
        , testCase
            "Sum type"
            ( equalPrettyLine "Int + Int" $ TyInt :+: TyInt
            )
        , testCase
            "Sum of sum right"
            ( equalPrettyLine "Int + (Unit + Bool)" $ TyInt :+: (TyUnit :+: TyBool)
            )
        , testCase
            "Sum of sum left"
            ( equalPrettyLine "(Int + Unit) + Bool" $ (TyInt :+: TyUnit) :+: TyBool
            )
        , testCase
            "Product of product right"
            ( equalPrettyLine "Int * (Unit * Bool)" $ TyInt :*: (TyUnit :*: TyBool)
            )
        , testCase
            "Product of product left"
            ( equalPrettyLine "(Int * Unit) * Bool" $ (TyInt :*: TyUnit) :*: TyBool
            )
        , testCase
            "Product of sum"
            ( equalPrettyLine "Int * (Unit + Bool)" $ TyInt :*: (TyUnit :+: TyBool)
            )
        , testCase
            "Sum of product"
            ( equalPrettyLine "Int + (Unit * Bool)" $ TyInt :+: (TyUnit :*: TyBool)
            )
        , testCase
            "Product of function"
            ( equalPrettyLine "Int * (Unit -> Bool)" $ TyInt :*: (TyUnit :->: TyBool)
            )
        , testCase
            "Function of product"
            ( equalPrettyLine "Int -> (Unit * Bool)" $ TyInt :->: (TyUnit :*: TyBool)
            )
        , testCase
            "Function of function right"
            ( equalPrettyLine "Int -> Unit -> Bool" $ TyInt :->: (TyUnit :->: TyBool)
            )
        , testCase
            "Function of function left"
            ( equalPrettyLine "(Int -> Unit) -> Bool" $ (TyInt :->: TyUnit) :->: TyBool
            )
        , testCase
            "density (two nested products)"
            ( equalPrettyLine "((Int * Int) * (Int * Int)) -> Cmd Int" $
                ((TyInt :*: TyInt) :*: (TyInt :*: TyInt)) :->: TyCmd TyInt
            )
        ]
    , testGroup
        "types but with limited width for pretty printing"
        [ testCase
            "Void type"
            ( equalPrettyWidth 10 "Void" TyVoid
            )
        , testCase
            "Function type"
            ( equalPrettyWidth 20 "Int -> Cmd Unit" $ TyInt :->: TyCmd TyUnit
            )
        , testCase
            "Cmd type"
            ( equalPrettyWidth 20 "Cmd (Int -> Int)" $ TyCmd (TyInt :->: TyInt)
            )
        , testCase
            "Product type"
            ( equalPrettyWidth 20 "Int * Int" $ TyInt :*: TyInt
            )
        , testCase
            "Function of function right"
            ( equalPrettyWidth 10 "Int ->\nUnit ->\nBool" $ TyInt :->: (TyUnit :->: TyBool)
            )
        , testCase
            "Function of function left"
            ( equalPrettyWidth 20 "(Int -> Unit) ->\nBool" $ (TyInt :->: TyUnit) :->: TyBool
            )
        , testCase
            "density (two nested products) with  nested indentation"
            ( equalPrettyWidth 20 "(\n  (Int * Int) * (\n    Int * Int\n  )\n) -> Cmd Int" $
                ((TyInt :*: TyInt) :*: (TyInt :*: TyInt)) :->: TyCmd TyInt
            )
        , testCase
            "Resonate"
            ( equalPrettyWidth 40 "Text -> ((Int * Int) * (Int * Int)) ->\nCmd (Unit + (Int * Int))" $
                TyText :->: ((TyInt :*: TyInt) :*: (TyInt :*: TyInt)) :->: TyCmd (TyUnit :+: (TyInt :*: TyInt))
            )
        ]
    , testGroup
        "tydef"
        [ testCase "tydef alias" $
            equalPrettyLine "tydef X = Int end" $
              TTydef "X" (Forall [] TyInt) Nothing (TConst Noop)
        , testCase "tydef Maybe" $
            equalPrettyLine "tydef Maybe a = Unit + a end" $
              TTydef "Maybe" (Forall ["a"] (TyUnit :+: TyVar "a")) Nothing (TConst Noop)
        , testCase "tydef multi-arg" $
            equalPrettyLine "tydef Foo a b c d = Unit + ((a * b) + ((c -> d) * a)) end" $
              TTydef
                "Foo"
                ( Forall
                    ["a", "b", "c", "d"]
                    (TyUnit :+: (TyVar "a" :*: TyVar "b") :+: ((TyVar "c" :->: TyVar "d") :*: TyVar "a"))
                )
                Nothing
                (TConst Noop)
        , testCase "consecutive tydef" $
            equalPrettyLine "tydef X = Int end\n\ntydef Y = Bool end" $
              TTydef "X" (Forall [] TyInt) Nothing (TTydef "Y" (Forall [] TyBool) Nothing (TConst Noop))
        ]
    , testGroup
        "recursive types"
        [ testCase "nat" $
            equalPrettyLine "rec n. Unit + n" $
              TyRec "n" (TyUnit :+: Fix (TyRecVarF NZ))
        , testCase "list" $
            equalPrettyLine "rec list. Unit + (a * list)" $
              TyRec "list" (TyUnit :+: (TyVar "a" :*: Fix (TyRecVarF NZ)))
        , testCase "rose" $
            equalPrettyLine "rec r. a * (rec l. Unit + (r * l))" $
              TyRec "r" (TyVar "a" :*: TyRec "l" (TyUnit :+: (Fix (TyRecVarF (NS NZ)) :*: Fix (TyRecVarF NZ))))
        ]
    ]
 where
  equalPrettyLine :: PrettyPrec a => String -> a -> Assertion
  equalPrettyLine expected = assertEqual "" expected . into @String . prettyTextLine

  equalPrettyWidth :: PrettyPrec a => Int -> String -> a -> Assertion
  equalPrettyWidth width expected doc = assertEqual "" expected . into @String $ prettyTextWidth doc width
