{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestLanguagePipeline where

import Control.Arrow ((&&&))
import Control.Lens (toListOf)
import Control.Lens.Plated (universe)
import Data.Aeson (eitherDecode, encode)
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Swarm.Language.Module (Module (..))
import Swarm.Language.Parse.QQ (tyQ)
import Swarm.Language.Pipeline (ProcessedTerm (..), processTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
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
    , testCase
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
        "Parse pair syntax #225"
        (valid "def f : (int -> bool) * (int -> bool) = (\\x. false, \\x. true) end")
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
                "2:7: Type mismatch:\n  From context, expected `\"oops\"` to have type `int`,\n  but it actually has type `text`"
            )
        , testCase
            "failure inside bind chain"
            ( process
                "move;\n1;\nmove"
                "2:1: Type mismatch:\n  From context, expected `1` to have type `cmd u0`,\n  but it actually has type `int`"
            )
        , testCase
            "failure inside function call"
            ( process
                "if true \n{} \n(move)"
                "3:1: Type mismatch:\n  From context, expected `move` to have type `{u0}`,\n  but it actually has type `cmd unit`"
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
        [ testCase "simple expr" (roundTrip "42 + 43")
        , testCase "module def" (roundTrip "def x = 41 end; def y = 42 end")
        ]
    , testGroup
        "atomic - #479"
        [ testCase
            "atomic move"
            ( valid "atomic move"
            )
        , testCase
            "grabif"
            (valid "def grabif : text -> cmd unit = \\x. atomic (b <- ishere x; if b {grab; return ()} {}) end")
        , testCase
            "placeif"
            (valid "def placeif : text -> cmd bool = \\thing. atomic (res <- scan down; if (res == inl ()) {place thing; return true} {return false}) end")
        , testCase
            "atomic move+move"
            ( process
                "atomic (move; move)"
                "1:8: Invalid atomic block: block could take too many ticks (2): move; move"
            )
        , testCase
            "atomic lambda"
            ( process
                "atomic ((\\c. c;c) move)"
                "1:9: Invalid atomic block: def, let, and lambda are not allowed: \\c. c; c"
            )
        , testCase
            "atomic non-simple"
            ( process
                "def dup = \\c. c; c end; atomic (dup (dup move))"
                "1:33: Invalid atomic block: reference to variable with non-simple type ∀ a. cmd a -> cmd a: dup"
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
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: wait"
            )
        , testCase
            "atomic make"
            ( process
                "atomic (make \"PhD thesis\")"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: make"
            )
        , testCase
            "atomic drill"
            ( process
                "atomic (drill forward)"
                "1:9: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: drill"
            )
        , testCase
            "atomic salvage"
            ( process
                "atomic (salvage)"
                "1:8: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: salvage"
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
        "void type"
        [ testCase
            "isSimpleUType"
            ( assertBool "" $ isSimpleUType UTyVoid
            )
        , testCase
            "valid type signature"
            (valid "def f : void -> a = \\x. undefined end")
        ]
    , testGroup
        "record type"
        [ testCase
            "valid record"
            (valid "\\x:int. ([y = \"hi\", x, z = \\x.x] : [x:int, y:text, z:bool -> bool])")
        , testCase
            "infer record type"
            (valid "[x = 3, y = \"hi\"]")
        , testCase
            "field mismatch - missing"
            ( process
                "(\\r:[x:int, y:int]. r.x) [x = 3]"
                "1:26: Field mismatch; record literal has:\n  - Missing field(s) `y`"
            )
        , testCase
            "field mismatch - extra"
            ( process
                "(\\r:[x:int, y:int]. r.x) [x = 3, y = 4, z = 5]"
                "1:26: Field mismatch; record literal has:\n  - Extra field(s) `z`"
            )
        , testCase
            "field mismatch - both"
            ( process
                "(\\r:[x:int, y:int]. r.x) [x = 3, z = 5]"
                "1:26: Field mismatch; record literal has:\n  - Extra field(s) `z`\n  - Missing field(s) `y`"
            )
        ]
    , testGroup
        "type annotations"
        [ testCase
            "annotate 1 + 1"
            ( assertEqual
                "type annotations"
                (toListOf traverse (getSyntax [tmQ| 1 + 1 |]))
                [[tyQ| int -> int -> int|], [tyQ|int|], [tyQ|int -> int|], [tyQ|int|], [tyQ|int|]]
            )
        , testCase
            "get all annotated variable types"
            ( let s =
                    getSyntax
                      [tmQ| def f : (int -> int) -> int -> text = \g. \x. format (g x) end |]

                  isVar (TVar {}) = True
                  isVar _ = False
                  getVars = map (_sTerm &&& _sType) . filter (isVar . _sTerm) . universe
               in assertEqual
                    "variable types"
                    (getVars s)
                    [ (TVar "g", [tyQ| int -> int |])
                    , (TVar "x", [tyQ| int |])
                    ]
            )
        , testCase
            "simple type ascription"
            (valid "(3 : int) + 5")
        , testCase
            "invalid type ascription"
            (process "1 : text" "1:1: Type mismatch:\n  From context, expected `1` to have type `text`,\n  but it actually has type `int`")
        , testCase
            "type ascription with a polytype"
            (valid "((\\x . x) : a -> a) 3")
        , testCase
            "type ascription too general"
            (process "1 : a" "1:1: Type mismatch:\n  From context, expected `1` to have type `s0`,\n  but it actually has type `int`")
        , testCase
            "type specialization through type ascription"
            (valid "fst:(int + b) * a -> int + b")
        , testCase
            "type ascription doesn't allow rank 2 types"
            ( process
                "\\f. (f:forall a. a->a) 3"
                "1:5: Skolem variable s3 would escape its scope"
            )
        , testCase
            "checking a lambda with the wrong argument type"
            ( process
                "(\\x:int. x + 2) : text -> int"
                "1:1: Lambda argument has type annotation int, but expected argument type text"
            )
        ]
    , testGroup
        "typechecking errors"
        [ testCase
            "applying a pair"
            ( process
                "(1,2) \"hi\""
                "1:1: Type mismatch:\n  From context, expected `(1, 2)` to have type `u3 -> u4`,\n  but it actually has type `u1 * u2`"
            )
        , testCase
            "providing a pair as an argument"
            ( process
                "(\\x:int. x + 1) (1,2)"
                "1:17: Type mismatch:\n  From context, expected `(1, 2)` to have type `int`,\n  but it actually has type `u0 * u1`"
            )
        , testCase
            "mismatched if branches"
            ( process
                "if true {grab} {}"
                "1:16: Type mismatch:\n  From context, expected `{}` to have type `cmd text`,\n  but it actually has type `cmd unit`"
            )
        , testCase
            "definition with wrong result"
            ( process
                "def m : int -> int -> int = \\x. \\y. {3} end"
                "1:37: Type mismatch:\n  From context, expected `{3}` to have type `int`,\n  but it actually has type `{u0}`"
            )
        ]
    ]

 where
  valid = flip process ""

  roundTrip txt = assertEqual "roundtrip" term (decodeThrow $ encode term)
   where
    decodeThrow v = case eitherDecode v of
      Left e -> error $ "Decoding of " <> from (T.decodeUtf8 (from v)) <> " failed with: " <> from e
      Right x -> x
    term = fromMaybe (error "") $ fromRight (error "") $ processTerm txt

  process :: Text -> Text -> Assertion
  process code expect = case processTerm code of
    Left e
      | not (T.null expect) && expect `T.isPrefixOf` e -> pure ()
      | otherwise -> error $ "Unexpected failure: " <> show e
    Right _
      | expect == "" -> pure ()
      | otherwise -> error "Unexpected success"

  getSyntax :: ProcessedTerm -> Syntax' Polytype
  getSyntax (ProcessedTerm (Module s _) _ _) = s
