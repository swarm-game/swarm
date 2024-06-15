{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestPretty where

import Data.Fix (Fix (..))
import Swarm.Language.Pretty
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
            TApp (TConst Neg) $
              TApp (TConst Neg) (TInt 1)
        )
    , testCase
        "operators #8 - unary negation with strongly fixing binary operator"
        ( equalPretty "-1 ^ (-2)"
            . TApp (TConst Neg)
            . mkOp' Exp (TInt 1)
            $ TApp (TConst Neg) (TInt 2)
        )
    , testCase
        "operators #8 - unary negation with weakly fixing binary operator"
        ( equalPretty "-(1 + -2)"
            . TApp (TConst Neg)
            . mkOp' Add (TInt 1)
            $ TApp (TConst Neg) (TInt 2)
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
    , testCase
        "pairs #225 - nested pairs are printed right-associative"
        ( equalPretty "(1, 2, 3)" $
            TPair (TInt 1) (TPair (TInt 2) (TInt 3))
        )
    , testCase
        "type ascription"
        ( equalPretty "1 : Int" $
            TAnnotate (TInt 1) (Forall [] TyInt)
        )
    , testCase
        "lambda precedence (#1468)"
        ( equalPretty "\\m. case m (\\x. x + 1) (\\y. y * 2)" $
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
            ( equalPretty "Void" TyVoid
            )
        , testCase
            "Unit type"
            ( equalPretty "Unit" TyUnit
            )
        , testCase
            "Function type"
            ( equalPretty "Int -> Cmd Unit" $ TyInt :->: TyCmd TyUnit
            )
        , testCase
            "Cmd type"
            ( equalPretty "Cmd (Int -> Int)" $ TyCmd (TyInt :->: TyInt)
            )
        , testCase
            "Cmd type"
            ( equalPretty "Cmd (Int -> Int)" $ TyCmd (TyInt :->: TyInt)
            )
        , testCase
            "Product type"
            ( equalPretty "Int * Int" $ TyInt :*: TyInt
            )
        , testCase
            "Sum type"
            ( equalPretty "Int + Int" $ TyInt :+: TyInt
            )
        , testCase
            "Sum of sum right"
            ( equalPretty "Int + (Unit + Bool)" $ TyInt :+: (TyUnit :+: TyBool)
            )
        , testCase
            "Sum of sum left"
            ( equalPretty "(Int + Unit) + Bool" $ (TyInt :+: TyUnit) :+: TyBool
            )
        , testCase
            "Product of product right"
            ( equalPretty "Int * (Unit * Bool)" $ TyInt :*: (TyUnit :*: TyBool)
            )
        , testCase
            "Product of product left"
            ( equalPretty "(Int * Unit) * Bool" $ (TyInt :*: TyUnit) :*: TyBool
            )
        , testCase
            "Product of sum"
            ( equalPretty "Int * (Unit + Bool)" $ TyInt :*: (TyUnit :+: TyBool)
            )
        , testCase
            "Sum of product"
            ( equalPretty "Int + (Unit * Bool)" $ TyInt :+: (TyUnit :*: TyBool)
            )
        , testCase
            "Product of function"
            ( equalPretty "Int * (Unit -> Bool)" $ TyInt :*: (TyUnit :->: TyBool)
            )
        , testCase
            "Function of product"
            ( equalPretty "Int -> (Unit * Bool)" $ TyInt :->: (TyUnit :*: TyBool)
            )
        , testCase
            "Function of function right"
            ( equalPretty "Int -> Unit -> Bool" $ TyInt :->: (TyUnit :->: TyBool)
            )
        , testCase
            "Function of function left"
            ( equalPretty "(Int -> Unit) -> Bool" $ (TyInt :->: TyUnit) :->: TyBool
            )
        , testCase
            "density (two nested products)"
            ( equalPretty "((Int * Int) * (Int * Int)) -> Cmd Int" $
                ((TyInt :*: TyInt) :*: (TyInt :*: TyInt)) :->: TyCmd TyInt
            )
        ]
    , testGroup
        "tydef"
        [ testCase "tydef alias" $
            equalPretty "tydef X = Int end" $
              TTydef (LV NoLoc "X") (Forall [] TyInt)
        , testCase "tydef Maybe" $
            equalPretty "tydef Maybe a = Unit + a end" $
              TTydef (LV NoLoc "Maybe") (Forall ["a"] (TyUnit :+: TyVar "a"))
        , testCase "tydef multi-arg" $
            equalPretty "tydef Foo a b c d = Unit + ((a * b) + ((c -> d) * a)) end" $
              TTydef
                (LV NoLoc "Foo")
                ( Forall
                    ["a", "b", "c", "d"]
                    (TyUnit :+: (TyVar "a" :*: TyVar "b") :+: ((TyVar "c" :->: TyVar "d") :*: TyVar "a"))
                )
        ]
    , testGroup
        "recursive types"
        [ testCase "nat" $
            equalPretty "rec n. Unit + n" $
              TyRec "n" (TyUnit :+: Fix (TyRecVarF NZ))
        , testCase "list" $
            equalPretty "rec list. Unit + (a * list)" $
              TyRec "list" (TyUnit :+: (TyVar "a" :*: Fix (TyRecVarF NZ)))
        , testCase "rose" $
            equalPretty "rec r. a * (rec l. Unit + (r * l))" $
              TyRec "r" (TyVar "a" :*: TyRec "l" (TyUnit :+: (Fix (TyRecVarF (NS NZ)) :*: Fix (TyRecVarF NZ))))
        ]
    ]
 where
  equalPretty :: PrettyPrec a => String -> a -> Assertion
  equalPretty expected = assertEqual "" expected . into @String . prettyTextLine
