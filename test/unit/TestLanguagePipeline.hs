{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestLanguagePipeline where

import Control.Arrow ((&&&))
import Control.Carrier.Error.Either (runError)
import Control.Lens (toListOf)
import Control.Lens.Plated (universe)
import Data.Aeson (eitherDecode, encode)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Swarm.Failure (SystemFailure)
import Swarm.Language.Load (SyntaxWithImports (..))
import Swarm.Language.JSON ()
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Parser.QQ (tyQ)
import Swarm.Language.Pipeline (processSource)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (isSimpleUType)
import Swarm.Language.Types
import Swarm.Pretty (prettyText)
import Test.Tasty
import Test.Tasty.HUnit
import Witch (from)

testLanguagePipeline :: TestTree
testLanguagePipeline =
  testGroup
    "Language - pipeline"
    [ testCase "end semicolon #79" (valid "def a = 41 end def b = a + 1 end def c = b + 2 end")
    , testGroup
        "quantification + scope"
        [ testCase
            "quantification #148 - implicit"
            (valid "def id : a -> a = \\x. x end; id move")
        , testCase
            "quantification #148 - explicit"
            (valid "def id : forall a. a -> a = \\x. x end; id move")
        , testCase
            "quantification #148 - explicit with free tyvars"
            (valid "def id : forall a. b -> b = \\x. x end; id move")
        , testCase
            "type variable scope #2178"
            (valid "def f : a -> (a * Int) = \\x. let g : a * Int = (x, 3) in g end")
        , testCase
            "type variable scope #2178 - shadowing"
            ( process
                "def f : a -> (a * Int) = \\x. let g : forall a. a * Int = (x, 3) in g end"
                "1:59: Type mismatch:\n  From context, expected `x` to have type `a`,\n  but it actually has type `a`"
            )
        ]
    , testCase
        "parsing operators #188 - parse valid operator (!=)"
        (valid "1!=(2)")
    , testCase
        "parsing operators #236 - parse valid operator (<=)"
        (valid "1 <= 2")
    , testCase
        "parsing operators #239 - parse valid operator ($)"
        (valid "(\\x. -x) $ (\\x. x^2) $ 2")
    , testGroup
        "Identifiers"
        [ testCase
            "Allow ' in variable names #269 - parse variable name containing '"
            (valid "def a'_' = 0 end")
        , testCase
            "Allow ' in variable names #269 - do not parse variable starting with '"
            ( process
                "def 'a = 0 end"
                ( T.unlines
                    [ "1:5:"
                    , "  |"
                    , "1 | def 'a = 0 end"
                    , "  |     ^"
                    , "unexpected '''"
                    , "expecting variable name"
                    ]
                )
            )
        , testCase
            "Disallow type name as variable name"
            ( process
                "let Int = 3 in Int + 1"
                ( T.unlines
                    [ "1:8:"
                    , "  |"
                    , "1 | let Int = 3 in Int + 1"
                    , "  |        ^"
                    , "Reserved word 'Int' cannot be used as a variable name"
                    ]
                )
            )
        , testCase
            "Allow uppercase term variable names"
            (valid "let Is = 3 in Is + 1")
        , testCase
            "Disallow uppercase type variable names"
            ( process
                "def id : A -> A = \\x. x end"
                ( T.init $
                    T.unlines
                      [ "1:1: Undefined type A"
                      , ""
                      , "  - While checking the definition of id"
                      ]
                )
            )
        , testCase
            "Allow term variable names which are lowercase versions of reserved type names"
            (valid "def idInt : Int -> Int = \\int. int end")
        , testCase
            "Disallow type variable names which are lowercase versions of reserved type names"
            ( process
                "def id : int -> int = \\x. x end"
                ( T.unlines
                    [ "1:13:"
                    , "  |"
                    , "1 | def id : int -> int = \\x. x end"
                    , "  |             ^"
                    , "Reserved type name 'int' cannot be used as a type variable name; perhaps you meant 'Int'?"
                    ]
                )
            )
        ]
    , testCase
        "Parse pair syntax #225"
        (valid "def f : (Int -> Bool) * (Int -> Bool) = (\\x. false, \\x. true) end")
    , testCase
        "Nested pair syntax"
        (valid "(1,2,3,4)")
    , testCase
        "Binder at end of block"
        (valid "r <- build {move}")
    , testGroup
        "failure location - #268"
        [ testCase
            "located type error"
            ( process
                "def a =\n 42 + \"oops\"\nend"
                "2:7: Type mismatch:\n  From context, expected `\"oops\"` to have type `Int`,\n  but it actually has type `Text`"
            )
        , testCase
            "failure inside bind chain"
            ( process
                "move;\n1;\nmove"
                "2:1: Type mismatch:\n  From context, expected `1` to be a command,\n  but it actually has type `Int`"
            )
        , testCase
            "failure inside function call"
            ( process
                "if true \n{} \n(move)"
                "3:2: Type mismatch:\n  From context, expected `move` to have type `{Cmd Unit}`,\n  but it actually has type `Cmd Unit`"
            )
        , testCase
            "parsing operators #236 - report failure on invalid operator start"
            ( process
                "1 <== 2"
                ( T.unlines
                    [ "1:3:"
                    , "  |"
                    , "1 | 1 <== 2"
                    , "  |   ^"
                    , "unexpected '<'"
                    ]
                )
            )
        ]
    , testGroup
        "require - #201"
        [ testCase
            "require device"
            (valid "require \"boat\"")
        , testCase
            "stock entities"
            (valid "stock 64 \"rock\"")
        , testCase
            "invalid syntax to require"
            ( process
                "require x"
                ( T.unlines
                    [ "1:9:"
                    , "  |"
                    , "1 | require x"
                    , "  |         ^"
                    , "unexpected 'x'"
                    , "expecting device name in double quotes"
                    ]
                )
            )
        , testCase
            "invalid syntax to stock n"
            ( process
                "stock 2 x"
                ( T.unlines
                    [ "1:9:"
                    , "  |"
                    , "1 | stock 2 x"
                    , "  |         ^"
                    , "unexpected 'x'"
                    , "expecting entity name in double quotes"
                    ]
                )
            )
        ]
    , testGroup
        "json encoding"
        [ testCase "simple expr" (roundTripTerm "42 + 43")
        , testCase "module def" (roundTripTerm "def x = 41 end\n\ndef y = 42 end")
        ]
    , testGroup
        "atomic - #479"
        [ testCase
            "atomic move"
            (valid "atomic {move}")
        , testCase
            "grabif"
            (valid "def grabif : Text -> Cmd Unit = \\x. atomic {b <- ishere x; if b {grab; pure ()} {}} end")
        , testCase
            "placeif"
            (valid "def placeif : Text -> Cmd Bool = \\thing. atomic {res <- scan down; if (res == inl ()) {place thing; pure true} {pure false}} end")
        , testCase
            "atomic move+move"
            ( process
                "atomic {move; move}"
                "1:8: Invalid atomic block: block could take too many ticks (2): `{move; move}`"
            )
        , testCase
            "atomic lambda"
            ( process
                "atomic {(\\c. c;c) move}"
                "1:10: Invalid atomic block: def, let, and lambda are not allowed: `\\c. c; c`"
            )
        , testCase
            "atomic non-simple"
            ( process
                "def dup = \\c. c; c end; atomic {dup (dup move)}"
                "1:33: Invalid atomic block: reference to variable with non-simple type ∀ a. Cmd a -> Cmd a: `dup`"
            )
        , testCase
            "atomic nested"
            ( process
                "atomic {move; atomic {if true {} {}}}"
                "1:15: Invalid atomic block: nested atomic block"
            )
        , testCase
            "atomic wait"
            ( process
                "atomic {wait 1}"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `wait`"
            )
        , testCase
            "atomic make"
            ( process
                "atomic {make \"PhD thesis\"}"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `make`"
            )
        , testCase
            "atomic drill"
            ( process
                "atomic {drill forward}"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `drill`"
            )
        , testCase
            "atomic salvage"
            ( process
                "atomic {salvage}"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `salvage`"
            )
        , testCase
            "atomic with comment #2412"
            (valid "atomic {if true {c <- scan down; /* COMMENT */ noop } {}}")
        ]
    , testGroup
        "integer literals"
        [ testCase
            "binary literal"
            (valid "0b1011011101")
        , testCase
            "invalid binary literal"
            (process "0b101201" "1:6:\n  |\n1 | 0b101201\n  |      ^\nunexpected '2'\n")
        , testCase
            "octal literal"
            (valid "0o3726")
        , testCase
            "invalid octal literal"
            (process "0o3826" "1:4:\n  |\n1 | 0o3826\n  |    ^\nunexpected '8'\n")
        , testCase
            "hex literal"
            (valid "0xabcD6F")
        , testCase
            "invalid hex literal"
            (process "0xabcD6G2" "1:8:\n  |\n1 | 0xabcD6G2\n  |        ^\nunexpected 'G'\n")
        ]
    , testGroup
        "Void type"
        [ testCase
            "isSimpleUType"
            (assertBool "" $ isSimpleUType UTyVoid)
        , testCase
            "valid type signature"
            (valid "def f : Void -> a = \\x. undefined end")
        ]
    , testGroup
        "record type"
        [ testCase
            "valid record"
            (valid "\\x:Int. ([y = \"hi\", x, z = \\x.x] : [x:Int, y:Text, z:Bool -> Bool])")
        , testCase
            "infer record type"
            (valid "[x = 3, y = \"hi\"]")
        , testCase
            "field mismatch - missing"
            ( process
                "(\\r:[x:Int, y:Int]. r.x) [x = 3]"
                "1:26: Field mismatch; record literal has:\n  - Missing field(s) `y`"
            )
        , testCase
            "field mismatch - extra"
            ( process
                "(\\r:[x:Int, y:Int]. r.x) [x = 3, y = 4, z = 5]"
                "1:26: Field mismatch; record literal has:\n  - Extra field(s) `z`"
            )
        , testCase
            "field mismatch - both"
            ( process
                "(\\r:[x:Int, y:Int]. r.x) [x = 3, z = 5]"
                "1:26: Field mismatch; record literal has:\n  - Extra field(s) `z`\n  - Missing field(s) `y`"
            )
        , testCase
            "type mismatch with record projection"
            ( process
                "\\x:Int. x.y"
                "1:9: Type mismatch:\n  From context, expected `x` to have a record type,\n  but it actually has type `Int`"
            )
        , testCase
            "inference failure with record projection"
            ( process
                "\\x. x.y"
                "1:5: In the record projection `x.y`, can't infer whether the LHS has a record type.  Try adding a type annotation."
            )
        , testCase
            "infer record projection with tydef"
            (valid "tydef R = [x:Int] end; def f : R -> Int = \\r. r.x end")
        , testCase
            "infer record projection with nested tydef"
            (valid "tydef B = [x:Int] end; tydef A = B end; def f : A -> Int = \\r. r.x end")
        , testCase
            "infer record projection with tydef and recursive type"
            (valid "tydef S = rec s. [hd:Int, tl:s] end; def two : S -> Int = \\s. s.tl.hd end")
        , testCase
            "infer record projection with tydef - RBTree"
            (valid "tydef Color = Bool end; tydef RBTree k v = rec b. Unit + [c: Color, k: k, v: v, l: b, r: b] end; def balanceLR : RBTree k v -> RBTree k v = \\ln. case ln undefined (\\ne. ne.r) end")
        , testCase
            "infer record projection with tydef - RBTree, error"
            ( process
                "tydef Color = Bool end; tydef RBTree k v = rec b. Unit + [c: Color, k: k, v: v, l: b, r: b] end; def balanceLR : RBTree k v -> RBTree k v = \\ln. case ln.r undefined undefined end"
                "1:151: Type mismatch:\n  From context, expected `ln` to have a record type,\n  but it actually has type `Unit +"
            )
        ]
    , testGroup
        "type annotations"
        [ testCase
            "annotate 1 + 1"
            ( assertEqual
                "type annotations"
                (toListOf (\f -> traverseSyntax f mempty) (getSyntax ([tmQ| 1 + 1 |] :: SyntaxWithImports Elaborated)))
                [[tyQ| Int -> Int -> Int|], [tyQ|Int|], [tyQ|Int -> Int|], [tyQ|Int|], [tyQ|Int|]]
            )
        , testCase
            "get all annotated variable types"
            ( let s = [tmQ| def f : (Int -> Int) -> Int -> Text = \g. \x. format (g x) end |]

                  isVar (TVar {}) = True
                  isVar _ = False
                  getVars :: Syntax Elaborated -> [(Term Elaborated, Polytype)]
                  getVars = map (_sTerm &&& _sType) . filter (isVar . _sTerm) . universe
               in assertEqual
                    "variable types"
                    (getVars (getSyntax s))
                    [ (TVar "g", [tyQ| Int -> Int |])
                    , (TVar "x", [tyQ| Int |])
                    ]
            )
        , testCase
            "simple type ascription"
            (valid "(3 : Int) + 5")
        , testCase
            "invalid type ascription"
            (process "1 : Text" "1:1: Type mismatch:\n  From context, expected `1` to have type `Text`,\n  but it actually has type `Int`")
        , testCase
            "type ascription with a polytype"
            (valid "((\\x . x) : a -> a) 3")
        , testCase
            "type ascription too general"
            (process "1 : a" "1:1: Type mismatch:\n  From context, expected `1` to have type `a`,\n  but it actually has type `Int`")
        , testCase
            "type specialization through type ascription"
            (valid "match : (Int + b) * a -> ((Int + b) -> a -> c) -> c")
        , testCase
            "type ascription doesn't allow rank 2 types"
            ( process
                "\\f. (f:forall a. a->a) 3"
                "1:24: Type mismatch:\n  From context, expected `3` to have type `a`,\n  but it actually has type `Int`"
            )
        , testCase
            "checking a lambda with the wrong argument type"
            ( process
                "(\\x:Int. x + 2) : Text -> Int"
                "1:2: Lambda argument has type annotation `Int`, but expected argument type `Text`"
            )
        ]
    , testGroup
        "kind checking"
        [ testCase
            "Cmd with no arguments"
            ( process
                "def x : Cmd = move end"
                ( T.init $
                    T.unlines
                      [ "1:1: Kind error:"
                      , "  Cmd requires 1 type argument, but was given 0"
                      , ""
                      , "  - While checking the definition of x"
                      ]
                )
            )
        , testCase
            "Cmd with too many arguments"
            ( process
                "def x : Cmd Int Unit = move end"
                ( T.init $
                    T.unlines
                      [ "1:1: Kind error:"
                      , "  Cmd requires 1 type argument, but was given 2"
                      , "  in the type: Cmd Int Unit"
                      , ""
                      , "  - While checking the definition of x"
                      ]
                )
            )
        , testCase
            "Base type applied to one argument"
            ( process
                "def isZero : Int Bool = \\n. n == 0 end"
                ( T.init $
                    T.unlines
                      [ "1:1: Kind error:"
                      , "  Int requires 0 type arguments, but was given 1"
                      , "  in the type: Int Bool"
                      , ""
                      , "  - While checking the definition of isZero"
                      ]
                )
            )
        , testCase
            "Base type applied to several arguments"
            ( process
                "def isZero : Int (Bool -> Bool) Text (Unit * Unit) = \\n. n == 0 end"
                ( T.init $
                    T.unlines
                      [ "1:1: Kind error:"
                      , "  Int requires 0 type arguments, but was given 3"
                      , "  in the type: Int (Bool -> Bool) Text (Unit * Unit)"
                      , ""
                      , "  - While checking the definition of isZero"
                      ]
                )
            )
        , testCase
            "Kind error in lambda type annotation"
            ( process
                "\\x : Int Int. x + 1"
                ( T.init $
                    T.unlines
                      [ "1:1: Kind error:"
                      , "  Int requires 0 type arguments, but was given 1"
                      , "  in the type: Int Int"
                      ]
                )
            )
        , testCase
            "Kind error in type annotation"
            ( process
                "(\\x. x) : Int Int"
                ( T.init $
                    T.unlines
                      [ "1:1: Kind error:"
                      , "  Int requires 0 type arguments, but was given 1"
                      , "  in the type: Int Int"
                      ]
                )
            )
        , testCase
            "Kind error in let expression"
            ( process
                "let x : Int Int = 3 in x + 5"
                ( T.init $
                    T.unlines
                      [ "1:1: Kind error:"
                      , "  Int requires 0 type arguments, but was given 1"
                      , "  in the type: Int Int"
                      ]
                )
            )
        ]
    , testGroup
        "typechecking errors"
        [ testCase
            "applying a pair"
            ( process
                "(1,2) \"hi\""
                "1:1: Type mismatch:\n  From context, expected `(1, 2)` to be a function,\n  but it actually has type `Int * Int`"
            )
        , testCase
            "providing a pair as an argument"
            ( process
                "(\\x:Int. x + 1) (1,2)"
                "1:17: Type mismatch:\n  From context, expected `(1, 2)` to have type `Int`,\n  but it is actually a pair"
            )
        , testCase
            "mismatched if branches"
            ( process
                "if true {grab} {}"
                "1:16: Type mismatch:\n  From context, expected `noop` to have type `Cmd Text`,\n  but it actually has type `Cmd Unit`"
            )
        , testCase
            "definition with wrong result"
            ( process
                "def m : Int -> Int -> Int = \\x. \\y. {3} end"
                "1:37: Type mismatch:\n  From context, expected `{3}` to have type `Int`,\n  but it is actually a delayed expression\n\n  - While checking the definition of m"
            )
        , testCase
            "comparing two incompatible functions"
            ( process
                "(\\f:Int -> Text. f 3) (\\x:Int. 3)"
                "1:32: Type mismatch:\n  From context, expected `3` to have type `Text`,\n  but it actually has type `Int`\n"
            )
        , testCase
            "comparing two incompatible functions 2"
            ( process
                "(\\f:Int -> Text. f 3) (\\x:Int. \\y:Int. \"hi\")"
                "1:32: Type mismatch:\n  From context, expected `\\y:Int. \"hi\"` to have type `Text`,\n  but it is actually a function\n"
            )
        , testCase
            "unify two-argument function and Int"
            ( process
                "1 + (\\x. \\y. 3)"
                "1:6: Type mismatch:\n  From context, expected `\\x. \\y. 3` to have type `Int`,\n  but it is actually a function\n"
            )
        , testCase
            "apply HOF to int - #1888"
            ( process
                "(\\f. f 3) 2"
                "1:11: Type mismatch:\n  From context, expected `2` to have a type like `Int -> _`"
            )
        , testCase
            "inferring type of bad recursive function - #2186"
            ( process
                "def fst = \\p. match p \\a. \\_. a end; def bad = \\acc. \\n. if (n <= 0) {fst acc} {bad (fst acc + 1) (n - 1)} end"
                "1:38: Type mismatch:\n  From context, expected `\\acc. \\n. if (n <= 0) {fst acc} {\n    bad (fst acc + 1) (n - 1)\n  }` to have type `Int -> Int -> Int`,\n  but it actually has a type like `(Int * _) -> Int -> Int`"
            )
        ]
    , testGroup
        "generalize top-level binds #351 #1501"
        [ testCase
            "top-level polymorphic bind is OK"
            (valid "r <- pure (\\x.x)")
        , testCase
            "top-level bind is polymorphic"
            (valid "f <- pure (\\x.x); pure (f 3, f \"hi\")")
        , testCase
            "local bind is polymorphic"
            (valid "def foo : Cmd (Int * Text) = f <- pure (\\x.x); pure (f 3, f \"hi\") end")
        ]
    , testGroup
        "type synonyms"
        [ testCase
            "X"
            (valid "tydef X = Int end; let n : X = 3 in log (format n)")
        , testCase
            "Maybe"
            (valid "tydef Maybe a = Unit + a end; let x : Maybe Int = inr 3 in case x (\\_. move) (\\n. log (format (n+2)))")
        , testCase
            "multi-args"
            ( valid $
                T.unlines
                  [ "tydef Foo a b c = a + (b * Int) + Cmd c end;"
                  , "let f1 : Foo (Cmd Unit) Int Text = inl move in"
                  , "  let f2 : Foo Int (Cmd Unit) Unit = inr (inl (move, 3)) in"
                  , "  let f3 : Foo Int Int Text = inr (inr grab) in"
                  , "  move"
                  ]
            )
        , testCase
            "multiple tydefs"
            (valid "tydef X = Int end; tydef Y = Unit end; def f : X -> Y = \\n. () end")
        , testCase
            "sequential tydefs don't need semicolon"
            (valid "tydef X = Int end tydef Y = Unit end")
        , testCase
            "sequential tydef + def don't need semicolon"
            (valid "tydef X = Int end def x : X = 5 end")
        , testCase
            "recursive tydef not allowed"
            ( process
                "tydef X = Unit + X end"
                "1:1: Undefined type X"
            )
        , testCase
            "nested tydef is allowed"
            (valid "def f : Cmd Unit = tydef X = Int end; move end")
        , testCase
            "tydef with repeated variables"
            ( process
                "tydef Repeated a b a = a + b end"
                "1:33:\n  |\n1 | tydef Repeated a b a = a + b end\n  |                                 ^\nDuplicate variable on left-hand side of tydef: a\n"
            )
        , testCase
            "tydef with unbound variables"
            ( process
                "tydef Unbound a b = a + b + c end"
                "1:34:\n  |\n1 | tydef Unbound a b = a + b + c end\n  |                                  ^\nUndefined type variable(s) on right-hand side of tydef: c\n"
            )
        , testCase
            "tydef shadowing #2437"
            ( process
                "tydef Foo = Int end; def f : Int -> Foo = \\x. x + 1 end; tydef Foo = Bool end; if (f 3) {} {}"
                "1:84: Type mismatch:\n  From context, expected `f 3` to have type `Bool`,\n  but it actually has type `Foo`"
            )
        , testCase
            "tydef shadowing #2437"
            ( valid
                "tydef Foo = Int end; def f : Int -> Foo = \\x. x + 1 end; tydef Foo = Bool end; def g : Int -> Foo = \\x. x > 5 end; if (g (f 3)) {} {}"
            )
        ]
    , testGroup
        "recursive types"
        [ testCase
            "occurs check"
            ( process
                "def sum = \\l. case l (\\_. 0) (\\c. match c \\hd. \\tl. hd + sum tl) end"
                "Encountered infinite type u6 = Int * (u5 + u6).\nSwarm will not infer recursive types; if you want a recursive type, add an explicit type annotation."
            )
        , testCase
            "no occurs check with type annotation"
            (valid "def sum : (rec l. Unit + Int * l) -> Int = \\l. case l (\\_. 0) (\\c. match c \\hd. \\tl. hd + sum tl) end")
        , testCase
            "vacuous"
            ( process
                "tydef X = rec x. x end"
                "1:1: Encountered vacuous recursive type rec x. x"
            )
        , testCase
            "nonobviously vacuous"
            ( process
                "tydef I a = a end; tydef M a b c = b end; tydef X = rec y. rec x. M x (I x) Int end"
                "1:43: Encountered vacuous recursive type rec x. M x (I x) Int"
            )
        , testCase
            "trivial"
            ( process
                "tydef X = rec x. Int end"
                "1:1: Encountered trivial recursive type rec x. Int"
            )
        , testCase
            "free rec vars"
            ( process
                "tydef X = rec y. rec x. y end"
                "1:1: Encountered trivial recursive type rec x. y"
            )
        , testCase
            "rec type with undefined tycon"
            ( process
                "tydef X = rec x. U + x end"
                "1:1: Undefined type U"
            )
        ]
    , testGroup
        "typechecking context stack"
        [ testCase
            "Stop printing context after a definition. - #1336"
            ( processCompare
                (==)
                "move; def x = move; say 3 end; move;"
                "1:25: Type mismatch:\n  From context, expected `3` to have type `Text`,\n  but it actually has type `Int`\n\n  - While checking the argument to a function: say _\n  - While checking the definition of x"
            )
        , testCase
            "Error inside function application + argument #2220"
            ( process
                "id 3 3"
                "1:1: Unbound variable id\n\n  - While checking a function applied to an argument: _ 3\n  - While checking a function applied to an argument: _ 3"
            )
        , testCase
            "Error inside function application + argument #2220"
            ( process
                "(\\x. x) 7 8"
                "1:1: Type mismatch:\n  From context, expected `(\\x. x) 7` to be a function,\n  but it actually has type `Int`\n\n  - While checking a function applied to an argument: _ 8"
            )
        , testCase
            "Nested error #2220"
            ( process
                "\"hi\" + 2"
                "1:1: Type mismatch:\n  From context, expected `\"hi\"` to have type `Int`,\n  but it actually has type `Text`\n\n  - While checking the argument to a function: (+) _\n  - While checking a function applied to an argument: _ 2"
            )
        ]
    , testGroup
        "let and def types"
        [ testCase
            "let at non-cmd type"
            (valid "let x = 3 in x + 2")
        , testCase
            "let at cmd type"
            (valid "let x = 3 in move; pure (x+2)")
        , testCase
            "def at non-cmd type"
            ( process
                "def x = 3 end; x + 2"
                "1:16: Type mismatch:\n  From context, expected `x + 2` to be a command"
            )
        , testCase
            "def at cmd type"
            (valid "def x = 3 end; move; pure (x+2)")
        ]
    , testGroup
        "nested let/def/annot #2101"
        [ testCase
            "nested polymorphic def/let"
            (valid "def id : a -> a = \\y. let x = 3 in y end")
        , testCase
            "nested polymorphic def/annot"
            (valid "def id : a -> a * Int = \\y. (y, 3 : Int) end")
        ]
    , testGroup
        "Custom error message for missing end #1141"
        [ testCase
            "missing end"
            ( process
                "def x = 3;\n def y = 3 end;\n def z = 3 end"
                "3:15:\n  |\n3 |  def z = 3 end\n  |               ^\nunexpected end of input\nexpecting \"!=\", \"&&\", \"()\", \"++\", \"<=\", \"==\", \">=\", \"def\", \"false\", \"import\", \"let\", \"require\", \"requirements\", \"stock\", \"true\", \"tydef\", \"||\", '\"', '$', '(', '*', '+', '-', '.', '/', ':', ';', '<', '>', '@', '[', '\\', '^', 'end' keyword for definition of 'x', '{', built-in user function, direction constant, integer literal, or variable name\n"
            )
        ]
    , testGroup
        "Typechecking for read #2461"
        [ testCase
            "read type argument propagates"
            ( process
                "(read @Int \"3\") 4"
                "1:1: Type mismatch:\n  From context, expected `(read @Int \"3\")` to be a function,\n  but it actually has type `Int`"
            )
        , testCase
            "read at type stored in a variable"
            ( process
                "def T : Type = @(Bool * Int) end; read T \"(True, 3)\""
                "1:35: The `read` command must be given a literal type as its first argument (Swarm does not have dependent types); found `T` instead."
            )
        ]
    , testGroup
        "Import #2540"
        [ testCase
            "simple import"
            ( valid "import \"data/test/import/a.sw\"; pure (a + 1)" )
        , testCase
            "recursive import - unused"
            ( valid "import \"data/test/import/b.sw\"; pure (b + 1)" )
        , testCase
            "recursive import - used"
            ( valid "import \"data/test/import/d.sw\"; pure (d + 1)" )
        , testCase
            "recursive import is not re-exported"
            ( process
               "import \"data/test/import/f.sw\"; pure (f + g)"
               "1:43: Unbound variable g"
            )
        ]
    ]
 where
  valid = flip process ""

  process :: Text -> Text -> Assertion
  process = processCompare T.isPrefixOf

  processCompare :: (Text -> Text -> Bool) -> Text -> Text -> Assertion
  processCompare cmp code expect = runError @SystemFailure (processSource code Nothing) >>= \case
    Left e
      | not (T.null expect) && cmp expect (prettyText e) -> pure ()
      | otherwise ->
          error $
            "Unexpected failure:\n\n  " <> show (prettyText e) <> "\n\nExpected:\n\n  " <> show expect <> "\n"
    Right _
      | expect == "" -> pure ()
      | otherwise -> error "Unexpected success"

-- | Check round tripping of term from and to text, then test ToJSON/FromJSON.
roundTripTerm :: Text -> Assertion
roundTripTerm txt = do
  assertEqual "roundtrip (readTerm -> prettyText)" txt (prettyText term)
  assertEqual "roundtrip (ToJSON -> FromJSON)" term (decodeThrow $ encode term)
 where
  decodeThrow v = case eitherDecode v of
    Left e -> error $ "Decoding of " <> from (T.decodeUtf8 (from v)) <> " failed with: " <> from e
    Right x -> x
  term = fromMaybe (error "empty document") $ either (error . T.unpack) id $ readTerm txt
