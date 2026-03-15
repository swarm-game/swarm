{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestEval where

import Control.Lens ((^.), _3)
import Data.Char (ord)
import Data.Fix
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty.Input.Events qualified as V
import Swarm.Game.State
import Swarm.Game.Value (Valuable (..))
import Swarm.Language.Array qualified as A
import Swarm.Language.Key
import Swarm.Language.Syntax.AST (Term (TInt))
import Swarm.Language.Syntax.Direction
import Swarm.Language.Types
import Swarm.Language.Value
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestUtil
import Witch (from, into)

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
            ("(\\x : Int + Bool + Text. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inl 3)" `evaluatesTo` VInt 1)
        , testCase
            "nested 2"
            ("(\\x : Int + Bool + Text. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inl false))" `evaluatesTo` VInt 2)
        , testCase
            "nested 2"
            ("(\\x : Int + Bool + Text. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inr \"hi\"))" `evaluatesTo` VInt 3)
        ]
    , testGroup
        "operator evaluation"
        [ testCase
            "application operator #239"
            ("(\\x. -x) $ (\\x. x^2) $ 2" `evaluatesTo` VInt (-4))
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
            ("try {pure 1} {pure 2}" `evaluatesTo` VInt 1)
        , testCase
            "try / no exception 2"
            ("try {pure 1} {let x = x in x}" `evaluatesTo` VInt 1)
        , testCase
            "try / fail"
            ("try {fail \"foo\"} {pure 3}" `evaluatesTo` VInt 3)
        , testCase
            "try / fail / fail"
            ("try {fail \"foo\"} {fail \"bar\"}" `throwsError` ("bar" `T.isInfixOf`))
        , testCase
            "try / div by 0"
            ("try {pure (1/0)} {pure 3}" `evaluatesTo` VInt 3)
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
            "format forall"
            ("format \"∀\"" `evaluatesTo` VText "\"∀\"")
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
        , testProperty
            "charAt"
            $ \i (NonEmpty s) ->
              let i' = i `mod` length s
               in T.concat ["charAt ", from @String (show i'), " ", tquote s]
                    `evaluatesToP` VInt (fromIntegral (ord (s !! i')))
        , testCase
            "toChar 97"
            ("toChar 97 == \"a\"" `evaluatesTo` VBool True)
        , testProperty
            "chars/toChar"
            ( \(NonNegative (i :: Integer)) ->
                T.concat ["chars (toChar ", from @String (show i), ")"]
                  `evaluatesToP` VInt 1
            )
        , testProperty
            "charAt/toChar"
            ( \(NonNegative i) ->
                T.concat ["charAt 0 (toChar ", from @String (show i), ")"]
                  `evaluatesToP` VInt i
            )
        ]
    , testGroup
        "read"
        [ testCase
            "read Unit"
            ("read @Unit \"()\"" `evaluatesToV` ())
        , testCase
            "read Unit with spaces"
            ("read @Unit \"   ()    \"" `evaluatesToV` ())
        , testCase
            "no read Unit"
            ("read @Unit \"xyz\"" `throwsError` ("Could not read" `T.isInfixOf`))
        , testCase
            "read Int"
            ("read @Int \"32\"" `evaluatesToV` (32 :: Integer))
        , testCase
            "read negative Int"
            ("read @Int \"-32\"" `evaluatesToV` (-32 :: Integer))
        , testCase
            "read Int with spaces"
            ("read @Int \"   -  32   \"" `evaluatesToV` (-32 :: Integer))
        , testCase
            "no read Int"
            ("read @Int \"32.0\"" `throwsError` ("Could not read" `T.isInfixOf`))
        , testCase
            "read false"
            ("read @Bool \"false\"" `evaluatesToV` False)
        , testCase
            "read true"
            ("read @Bool \"true\"" `evaluatesToV` True)
        , testCase
            "read forward"
            ( "read @Dir \"forward\""
                `evaluatesTo` VDir (DRelative (DPlanar DForward))
            )
        , testCase
            "read east"
            ("read @Dir \"east\"" `evaluatesTo` VDir (DAbsolute DEast))
        , testCase
            "read down"
            ("read @Dir \"down\"" `evaluatesTo` VDir (DRelative DDown))
        , testCase
            "read up"
            ("read @Dir \"up\"" `evaluatesTo` VDir (DRelative DUp))
        , testCase
            "read text"
            ("read @Text \"\\\"hi\\\"\"" `evaluatesToV` ("hi" :: Text))
        , testCase
            "read sum inl"
            ( "read @(Int + Bool) \"inl 3\""
                `evaluatesToV` Left @Integer @Bool 3
            )
        , testCase
            "read sum inr"
            ( "read @(Int + Bool) \"inr true\""
                `evaluatesToV` Right @Integer True
            )
        , testCase
            "read nested sum"
            ( "read @((Int + Bool) + Unit) \"inl (inr true)\""
                `evaluatesToV` Left @_ @() (Right @Integer True)
            )
        , testCase
            "read pair"
            ( "read @(Int * Bool) \"(3, true)\""
                `evaluatesToV` (3 :: Integer, True)
            )
        , testCase
            "read pair with non-atomic value"
            ( "read @(Int * (Unit + Bool)) \"(3, inr true)\""
                `evaluatesToV` (3 :: Integer, Right @() True)
            )
        , testCase
            "read nested pair"
            ( "read @(Int * Bool * Unit) \"(3, true, ())\""
                `evaluatesToV` (3 :: Integer, (True, ()))
            )
        , testCase
            "read left-nested pair"
            ( "read @((Int * Bool) * Unit) \"((3, true), ())\""
                `evaluatesToV` ((3 :: Integer, True), ())
            )
        , testCase
            "read empty record"
            ("read @[] \"[]\"" `evaluatesTo` VRcd M.empty)
        , testCase
            "read singleton record"
            ( "read @[x : Int] \"[x = 2]\""
                `evaluatesTo` VRcd (M.singleton "x" (VInt 2))
            )
        , testCase
            "read doubleton record"
            ( "read @[x : Int, y : Bool + Unit] \"[x = 2, y = inr ()]\""
                `evaluatesTo` (VRcd . M.fromList $ [("x", VInt 2), ("y", VInj True VUnit)])
            )
        , testCase
            "read permuted doubleton record"
            ( "read @[x : Int, y : Bool + Unit] \"[y = inr (), x = 2]\""
                `evaluatesTo` (VRcd . M.fromList $ [("x", VInt 2), ("y", VInj True VUnit)])
            )
        , testCase
            "no read record with repeated fields"
            ( "read @[x : Int] \"[x = 2, x = 3]\""
                `throwsError` ("Could not read" `T.isInfixOf`)
            )
        , testCase
            "read key"
            ( "read @Key \"key \\\"M-C-F5\\\"\""
                `evaluatesTo` VKey (mkKeyCombo [V.MCtrl, V.MMeta] (V.KFun 5))
            )
        , testCase
            "read recursive list"
            ( "read @(rec l. Unit + (Int * l)) \"inr (3, inr (5, inl ()))\""
                `evaluatesToV` [3 :: Integer, 5]
            )
        , testCase
            "read paper with int"
            ("read @Int \"paper: 52\"" `evaluatesToV` (52 :: Integer))
        , testCase
            "read paper with tuple"
            ( "read @(Int * Bool * Unit) \"paper: (3, false, ())\""
                `evaluatesToV` (3 :: Integer, (False, ()))
            )
        , testCase
            "read random entity with tuple"
            ( "read @(Int * Bool * Unit) \"foo: (3, false, ())\""
                `evaluatesToV` (3 :: Integer, (False, ()))
            )
        , testCase
            "read Text value containing colon"
            ( "read @Text \"\\\"hi: there\\\"\""
                `evaluatesToV` ("hi: there" :: Text)
            )
        , testCase
            "read at user-defined type Foo = Int"
            ( "tydef Foo = Int end; read @Foo \"3\""
                `evaluatesToV` (3 :: Integer)
            )
        , testCase
            "read at user-defined recursive type"
            ( "tydef List a = rec l. Unit + a * l end; read @(List(Int)) \"inr (1, inl ())\""
                `evaluatesToV` [1 :: Integer]
            )
        , testCase
            "read at complex type with user-defined types"
            ( "tydef Foo = Int end; read @(Bool * Foo) \"(True, 3)\""
                `evaluatesToV` (True, 3 :: Integer)
            )
        , testCase
            "read at nested user-defined type"
            ( "tydef A = Int end; tydef B = Bool end; tydef C = B * A end; read @C \"(True, 3)\""
                `evaluatesToV` (True, 3 :: Integer)
            )
        ]
    , testGroup
        "records - #1093"
        [ testCase
            "empty record"
            ("[]" `evaluatesTo` VRcd M.empty)
        , testCase
            "singleton record"
            ("[y = 3 + 4]" `evaluatesTo` VRcd (M.singleton "y" (VInt 7)))
        , testCase
            "record equality up to reordering"
            ("[x = 2, y = 3] == [y = 3, x = 2]" `evaluatesTo` VBool True)
        , testCase
            "record projection"
            ("[x = 2, y = 3].x" `evaluatesTo` VInt 2)
        , testCase
            "nested record projection"
            ("let r = [x=2, y=3] in let z = [q = r, n=\"hi\"] in z.q.y" `evaluatesTo` VInt 3)
        , testCase
            "record punning"
            ( "let x = 2 in let y = 3 in [x,y,z=\"hi\"]"
                `evaluatesTo` VRcd (M.fromList [("x", VInt 2), ("y", VInt 3), ("z", VText "hi")])
            )
        , testCase
            "record comparison"
            ("[y=1, x=2] < [x=3,y=0]" `evaluatesTo` VBool True)
        , testCase
            "record comparison"
            ("[y=1, x=3] < [x=3,y=0]" `evaluatesTo` VBool False)
        , testCase
            "record function"
            ("let f : [x:Int, y:Text] -> Int = \\r.r.x + 1 in f [x=3,y=\"hi\"]" `evaluatesTo` VInt 4)
        , testCase
            "format record"
            ("format [y = 2, x = 1+2]" `evaluatesTo` VText "[x = 3, y = 2]")
        , testCase
            "record fields don't scope over other fields"
            ( "let x = 1 in [x = x + 1, y = x]"
                `evaluatesTo` VRcd (M.fromList [("x", VInt 2), ("y", VInt 1)])
            )
        ]
    , testGroup
        "scope - #681"
        [ testCase
            "binder in local scope"
            ("def f = a <- scan down end; let a = 2 in f; pure (a+1)" `evaluatesTo` VInt 3)
        , testCase
            "binder in local scope, no type change"
            ("def f = a <- pure 1 end; let a = 2 in f; pure a" `evaluatesTo` VInt 2)
        , testCase
            "repeat with scan"
            ("def x = \\n. \\c. if (n==0) {} {c; x (n-1) c} end; x 10 ( c <- scan down; case c (\\_. say \"Hi\") (\\_. pure ()))" `evaluatesTo` VUnit)
        , testCase
            "nested recursion with binder - #1032"
            ("def go = \\n. if (n > 0) {i <- pure n; s <- go (n-1); pure (s+i)} {pure 0} end; go 4" `evaluatesTo` VInt 10)
        , testCase
            "binder in local scope - #1796"
            ("def x = \\x.x end; def foo = x <- pure 0 end; foo; pure (x 42)" `evaluatesTo` VInt 42)
        ]
    , testGroup
        "nesting"
        [ testCase
            "def nested in def"
            ("def x : Cmd Int = def y : Int = 3 end; pure (y + 2) end; x" `evaluatesTo` VInt 5)
        , testCase
            "nested def does not escape"
            ( "def z = 1 end; def x = def z = 3 end; pure (z + 2) end; n <- x; pure (n + z)"
                `evaluatesTo` VInt 6
            )
        , testCase
            "nested tydef"
            ( "def x = (tydef X = Int end; def z : X = 3 end; pure (z + 2)) end; x"
                `evaluatesTo` VInt 5
            )
        ]
    , testCase
        "tydef does not prevent forcing of recursive variables"
        ("def forever = \\c. c; forever c end; tydef X = Int end; def go = forever move end" `evaluatesTo` VUnit)
    , testGroup
        "term-level types"
        [ testCase
            "Int"
            ("@Int" `evaluatesTo` VType TyInt)
        , testCase
            "Int * Bool"
            ("@(Int * Bool)" `evaluatesTo` VType (TyInt :*: TyBool))
        , testCase
            "foo"
            ("@(rec a. a)" `evaluatesTo` VType (TyRec "a" (Fix (TyRecVarF NZ))))
        , testCase
            "list"
            ("@(rec l. Unit + Int * l)" `evaluatesTo` VType (TyRec "l" (TyUnit :+: (TyInt :*: Fix (TyRecVarF NZ)))))
        ]
    , testGroup
        "default"
        [ testCase
            "Unit"
            ("default @Unit" `evaluatesTo` VUnit)
        , testCase
            "Int"
            ("default @Int" `evaluatesTo` VInt 0)
        , testCase
            "Text"
            ("default @Text" `evaluatesTo` VText "")
        , testCase
            "Dir"
            ("default @Dir" `evaluatesTo` VDir (DRelative (DPlanar DForward)))
        , testCase
            "Bool"
            ("default @Bool" `evaluatesTo` VBool False)
        , testCase
            "Actor"
            ("default @Actor" `evaluatesTo` VRobot 0)
        , testCase
            "Key"
            ("default @Key" `evaluatesTo` VKey (mkKeyCombo [] (V.KChar ' ')))
        , testCase
            "Type"
            ("default @Type" `evaluatesTo` VType TyUnit)
        , testCase
            "Unit + Bool"
            ("default @(Unit + Bool)" `evaluatesTo` VInj False VUnit)
        , testCase
            "Void + Unit"
            ("default @(Void + Unit)" `evaluatesTo` VInj True VUnit)
        , testCase
            "Void + (Void + Unit)"
            ("default @(Void + (Void + Unit))" `evaluatesTo` VInj True (VInj True VUnit))
        , testCase
            "Bool * Int"
            ("default @(Bool * Int)" `evaluatesTo` VPair (VBool False) (VInt 0))
        , testCase
            "Bool * (Int * Text)"
            ("default @(Bool * (Int * Text))" `evaluatesTo` VPair (VBool False) (VPair (VInt 0) (VText "")))
        , testCase
            "Int -> Bool"
            ("default @(Int -> Bool) 3" `evaluatesTo` VBool False)
        , testCase
            "record"
            ("default @[x: Int, y: Text, z: Bool]" `evaluatesTo` VRcd (M.fromList [("x", VInt 0), ("y", VText ""), ("z", VBool False)]))
        , testCase
            "Cmd Int"
            ("default @(Cmd Int)" `evaluatesTo` VInt 0)
        , testCase
            "{Int}"
            ("default @{Int}" `evaluatesTo` VDelay (TInt 0) emptyEnv)
        , testCase
            "tydef"
            ("tydef X = [x: Int, y: Text, z: Bool] end; default @X" `evaluatesTo` VRcd (M.fromList [("x", VInt 0), ("y", VText ""), ("z", VBool False)]))
        , testCase
            "nested tydef"
            ("tydef X = [x: Int, y: Text, z: Bool] end; tydef Y = Int * X end; default @Y" `evaluatesTo` VPair (VInt 0) (VRcd (M.fromList [("x", VInt 0), ("y", VText ""), ("z", VBool False)])))
        , testCase
            "recursive type (left)"
            ("default @(rec l. Unit + Int * l)" `evaluatesTo` VInj False VUnit)
        , testCase
            "recursive type (right)"
            ("default @(rec l. Int * l + Unit)" `evaluatesTo` VInj True VUnit)
        , testCase
            "recursive type (rose tree)"
            ("default @(rec t. Int * (rec l. Unit + t * l))" `evaluatesTo` VPair (VInt 0) (VInj False VUnit))
        ]
    , testGroup
        "import/export"
        [ testCase
            "shadow re-export"
            ("import \"data/test/export/shadow/a.sw\" in (a, b)" `evaluatesTo` VPair (VInt 3) (VInt 5))
        , testCase
            "module re-export shadows"
            ("import \"data/test/export/shadow/c.sw\" in d" `evaluatesTo` VInt 3)
        , testCase
            "individual re-export shadows"
            ("import \"data/test/export/shadow/e.sw\" in f" `evaluatesTo` VInt 3)
        ]
    , testGroup
        "arrays"
        [ testCase
            "array literal"
            ("[| 1, 2+3, 4 |]" `evaluatesTo` mkArray (map VInt [1, 5, 4]))
        , testCase
            "empty array literal"
            ("[| |]" `evaluatesTo` mkArray [])
        , testCase
            "singleton array literal"
            ("[| \"hi\" |]" `evaluatesTo` mkArray [VText "hi"])
        , testCase
            "nested array literal"
            ("[| [| 1, 2 |], [| 3, 4 |] |]" `evaluatesTo` mkArray [mkArray [VInt 1, VInt 2], mkArray [VInt 3, VInt 4]])
        , testCase
            "array size"
            ("size [| 3, 5, 4 |]" `evaluatesTo` VInt 3)
        , testCase
            "size of empty array"
            ("size [||]" `evaluatesTo` VInt 0)
        , testCase
            "unfoldArray"
            ("unfoldArray (\\n. if (n==0) {inl()} {inr(n, n-1)}) 4" `evaluatesTo` mkArray (map VInt [4, 3, 2, 1]))
        , testCase
            "fromList"
            ("tydef List a = rec l. Unit + a * l end; def id = \\x. x end; def fromList = unfoldArray (id : List a -> Unit + a * List a) end; pure $ fromList (inr (4, inr (6, inl ())))" `evaluatesTo` mkArray (map VInt [4, 6]))
        , testCase
            "generate"
            ("def generate : Int -> (Int -> a) -> Array a = \\n. \\f. unfoldArray (\\k. if (k == n) {inl ()} {inr (f k, k + 1)}) 0 end; pure $ generate 4 (\\k. k*k)" `evaluatesTo` mkArray (map VInt [0, 1, 4, 9]))
        ]
    ]
 where
  tquote :: String -> Text
  tquote = T.pack . show

  mkArray :: [Value] -> Value
  mkArray = VArray . A.fromList

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
    assertBool (into @String $ either id (prettyValue . fst) result) $
      Right val == (fst <$> result)

  evaluatesToV :: Valuable v => Text -> v -> Assertion
  evaluatesToV tm val = tm `evaluatesTo` asValue val

  evaluatesToP :: Text -> Value -> Property
  evaluatesToP tm val = ioProperty $ do
    result <- evaluate tm
    return . property $ Right val == (fst <$> result)

  evaluatesToInAtMost :: Text -> (Value, Int) -> Assertion
  evaluatesToInAtMost tm (val, maxSteps) = do
    result <- evaluate tm
    case result of
      Left err -> assertFailure ("Evaluation failed: " ++ from @Text @String err)
      Right (v, steps) -> do
        assertBool "Values are not equal" (val == v)
        assertBool ("Took more than " ++ show maxSteps ++ " steps!") (steps <= maxSteps)

  evaluate :: Text -> IO (Either Text (Value, Int))
  evaluate = fmap (^. _3) . eval g

binOp :: Show a => a -> String -> a -> Text
binOp a op b = from @String (p (show a) ++ op ++ p (show b))
 where
  p x = "(" ++ x ++ ")"
