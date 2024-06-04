{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestLanguagePipeline where

import Control.Arrow ((&&&))
import Control.Lens (toListOf, view)
import Control.Lens.Plated (universe)
import Data.Aeson (eitherDecode, encode)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Parser.QQ (tyQ)
import Swarm.Language.Pipeline (processTerm, processedSyntax)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (isSimpleUType)
import Swarm.Language.Types
import Test.Tasty
import Test.Tasty.HUnit
import Witch (from)

testLanguagePipeline :: TestTree
testLanguagePipeline =
  testGroup
    "Language - pipeline"
    [ testCase "end semicolon #79" (valid "def a = 41 end def b = a + 1 end def c = b + 2 end")
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
    , testCase
        "parsing operators #188 - parse valid operator (!=)"
        (valid "1!=(2)")
    , testCase
        "parsing operators #236 - parse valid operator (<=)"
        (valid "1 <= 2")
    , testCase
        "parsing operators #239 - parse valid operator ($)"
        (valid "fst $ snd $ (1,2,3)")
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
                "3:1: Type mismatch:\n  From context, expected `move` to have type `{Cmd Unit}`,\n  but it actually has type `Cmd Unit`"
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
            "require entities"
            (valid "require 64 \"rock\"")
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
                    , "expecting device name in double quotes or integer literal"
                    ]
                )
            )
        , testCase
            "invalid syntax to require n"
            ( process
                "require 2 x"
                ( T.unlines
                    [ "1:11:"
                    , "  |"
                    , "1 | require 2 x"
                    , "  |           ^"
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
            ( valid "atomic move"
            )
        , testCase
            "grabif"
            (valid "def grabif : Text -> Cmd Unit = \\x. atomic (b <- ishere x; if b {grab; return ()} {}) end")
        , testCase
            "placeif"
            (valid "def placeif : Text -> Cmd Bool = \\thing. atomic (res <- scan down; if (res == inl ()) {place thing; return true} {return false}) end")
        , testCase
            "atomic move+move"
            ( process
                "atomic (move; move)"
                "1:8: Invalid atomic block: block could take too many ticks (2): `move; move`"
            )
        , testCase
            "atomic lambda"
            ( process
                "atomic ((\\c. c;c) move)"
                "1:9: Invalid atomic block: def, let, and lambda are not allowed: `\\c. c; c`"
            )
        , testCase
            "atomic non-simple"
            ( process
                "def dup = \\c. c; c end; atomic (dup (dup move))"
                "1:33: Invalid atomic block: reference to variable with non-simple type ∀ a. Cmd a -> Cmd a: `dup`"
            )
        , testCase
            "atomic nested"
            ( process
                "atomic (move; atomic (if true {} {}))"
                "1:15: Invalid atomic block: nested atomic block"
            )
        , testCase
            "atomic wait"
            ( process
                "atomic (wait 1)"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `wait`"
            )
        , testCase
            "atomic make"
            ( process
                "atomic (make \"PhD thesis\")"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `make`"
            )
        , testCase
            "atomic drill"
            ( process
                "atomic (drill forward)"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `drill`"
            )
        , testCase
            "atomic salvage"
            ( process
                "atomic (salvage)"
                "1:8: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: `salvage`"
            )
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
            ( assertBool "" $ isSimpleUType UTyVoid
            )
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
        ]
    , testGroup
        "type annotations"
        [ testCase
            "annotate 1 + 1"
            ( assertEqual
                "type annotations"
                (toListOf traverse (view processedSyntax [tmQ| 1 + 1 |]))
                [[tyQ| Int -> Int -> Int|], [tyQ|Int|], [tyQ|Int -> Int|], [tyQ|Int|], [tyQ|Int|]]
            )
        , testCase
            "get all annotated variable types"
            ( let s =
                    view
                      processedSyntax
                      [tmQ| def f : (Int -> Int) -> Int -> Text = \g. \x. format (g x) end |]

                  isVar (TVar {}) = True
                  isVar _ = False
                  getVars = map (_sTerm &&& _sType) . filter (isVar . _sTerm) . universe
               in assertEqual
                    "variable types"
                    (getVars s)
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
            (process "1 : a" "1:1: Type mismatch:\n  From context, expected `1` to have type `s0`,\n  but it actually has type `Int`")
        , testCase
            "type specialization through type ascription"
            (valid "fst:(Int + b) * a -> Int + b")
        , testCase
            "type ascription doesn't allow rank 2 types"
            ( process
                "\\f. (f:forall a. a->a) 3"
                "1:5: Skolem variable s3 would escape its scope"
            )
        , testCase
            "checking a lambda with the wrong argument type"
            ( process
                "(\\x:Int. x + 2) : Text -> Int"
                "1:1: Lambda argument has type annotation `Int`, but expected argument type `Text`"
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
                "1:1: Type mismatch:\n  From context, expected `(1, 2)` to be a function,\n  but it is actually a pair"
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
                "1:5: Type mismatch:\n  From context, expected `\\x. \\y. 3` to have type `Int`,\n  but it is actually a function\n"
            )
        ]
    , testGroup
        "generalize top-level binds #351 #1501"
        [ testCase
            "top-level polymorphic bind is OK"
            (valid "r <- return (\\x.x)")
        , testCase
            "top-level bind is polymorphic"
            (valid "f <- return (\\x.x); return (f 3, f \"hi\")")
        , testCase
            "local bind is polymorphic"
            (valid "def foo : Cmd (Int * Text) = f <- return (\\x.x); return (f 3, f \"hi\") end")
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
            "nested tydef not allowed"
            ( process
                "def f : Cmd Unit = tydef X = Int end; move end"
                "1:20: Definitions may only be at the top level: `tydef X = Int end`\n\n  - While checking the left-hand side of a semicolon\n  - While checking the definition of f"
            )
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
        ]
    , testGroup
        "recursive types"
        [ testCase
            "occurs check"
            ( process
                "def sum = \\l. case l (\\_. 0) (\\c. fst c + sum (snd c)) end"
                "Encountered infinite type u5 = Int * (u4 + u5).\nSwarm will not infer recursive types; if you want a recursive type, add an explicit type annotation."
            )
        , testCase
            "no occurs check with type annotation"
            (valid "def sum : (rec l. Unit + Int * l) -> Int = \\l. case l (\\_. 0) (\\c. fst c + sum (snd c)) end")
        ]
    ]
 where
  valid = flip process ""

  process :: Text -> Text -> Assertion
  process code expect = case processTerm code of
    Left e
      | not (T.null expect) && expect `T.isPrefixOf` e -> pure ()
      | otherwise ->
          error $
            "Unexpected failure:\n\n  " <> show e <> "\n\nExpected:\n\n  " <> show expect <> "\n"
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
