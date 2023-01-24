{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Swarm unit tests
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
import Swarm.Language.Module (Module(..))
import Swarm.Language.Pipeline (ProcessedTerm(..), processTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Parse.QQ (tyQ)
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
                "2: Can't unify int and text"
            )
        , testCase
            "failure inside bind chain"
            ( process
                "move;\n1;\nmove"
                "2: Can't unify int and cmd"
            )
        , testCase
            "failure inside function call"
            ( process
                "if true \n{} \n(move)"
                "3: Can't unify {u0} and cmd unit"
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
                "1: Invalid atomic block: block could take too many ticks (2): move; move"
            )
        , testCase
            "atomic lambda"
            ( process
                "atomic ((\\c. c;c) move)"
                "1: Invalid atomic block: def, let, and lambda are not allowed: \\c. c; c"
            )
        , testCase
            "atomic non-simple"
            ( process
                "def dup = \\c. c; c end; atomic (dup (dup move))"
                "1: Invalid atomic block: reference to variable with non-simple type ∀ a3. cmd a3 -> cmd a3: dup"
            )
        , testCase
            "atomic nested"
            ( process
                "atomic (move; atomic (if true {} {}))"
                "1: Invalid atomic block: nested atomic block"
            )
        , testCase
            "atomic wait"
            ( process
                "atomic (wait 1)"
                "1: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: wait"
            )
        , testCase
            "atomic make"
            ( process
                "atomic (make \"PhD thesis\")"
                "1: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: make"
            )
        , testCase
            "atomic drill"
            ( process
                "atomic (drill forward)"
                "1: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: drill"
            )
        , testCase
            "atomic salvage"
            ( process
                "atomic (salvage)"
                "1: Invalid atomic block: commands that can take multiple ticks to execute are not allowed: salvage"
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
            "void - isSimpleUType"
            ( assertBool "" $ isSimpleUType UTyVoid
            )
        , testCase
            "void - valid type signature"
            (valid "def f : void -> a = \\x. undefined end")
        ]
    , testGroup
        "type annotations"
        [ testCase
            "annotate 1 + 1"
            (assertEqual "type annotations"
               (toListOf traverse (getSyntax [tmQ| 1 + 1 |]))
               (map (Forall []) [TyInt :->: TyInt :->: TyInt, TyInt, TyInt :->: TyInt, TyInt, TyInt])
            )
        , testCase
            "get all annotated variable types"
            (let s = getSyntax
                   [tmQ| def f : (int -> int) -> int -> text = \g. \x. format (g x) end |]

                 isVar (TVar {}) = True
                 isVar _ = False
                 getVars = map (_sTerm &&& _sType) . filter (isVar . _sTerm) . universe
             in assertEqual "variable types"
                  (getVars s)
                  ([ (TVar "g", [tyQ| int -> int |])
                   , (TVar "x", Forall [] TyInt)
                   ]
                  )
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
