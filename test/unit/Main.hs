{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm unit tests
module Main where

import Control.Lens (Getter, use, (&), (.~), (^.), _3)
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson (eitherDecode, encode)
import Data.Either
import Data.Hashable
import Data.List (subsequences)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Poly qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Witch (from)

import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity qualified as E
import Swarm.Game.Exception
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Step (hypotheticalRobot, stepCESK)
import Swarm.Game.Value
import Swarm.Language.Context
import Swarm.Language.Pipeline (ProcessedTerm (..), processTerm)
import Swarm.Language.Pretty
import Swarm.Language.Syntax hiding (mkOp)
import Swarm.TUI.Model
import Swarm.Util

main :: IO ()
main = do
  mg <- runExceptT classicGame0
  case mg of
    Left err -> assertFailure (from err)
    Right g -> defaultMain (tests g)

tests :: GameState -> TestTree
tests g =
  testGroup
    "Tests"
    [ testParser
    , testPrettyConst
    , testEval g
    , testModel
    , testInventory
    , testNotification g
    , testMisc
    ]

testParser :: TestTree
testParser =
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
                "2: Can't unify int and string"
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
                "3: Can't unify {u0} and cmd ()"
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
            (valid "def grabif : string -> cmd () = \\x. atomic (b <- ishere x; if b {grab; return ()} {}) end")
        , testCase
            "placeif"
            (valid "def placeif : string -> cmd bool = \\thing. atomic (res <- scan down; if (res == inl ()) {place thing; return true} {return false}) end")
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
    ]
 where
  valid = flip process ""

  roundTrip txt = assertEqual "rountrip" term (decodeThrow $ encode term)
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
    , testCase
        "pairs #225 - nested pairs are printed right-associative"
        ( equalPretty "(1, 2, 3)" $
            TPair (TInt 1) (TPair (TInt 2) (TInt 3))
        )
    ]
 where
  equalPretty :: String -> Term -> Assertion
  equalPretty expected term = assertEqual "" expected . show $ ppr term

binOp :: Show a => a -> String -> a -> Text
binOp a op b = from @String (p (show a) ++ op ++ p (show b))
 where
  p x = "(" ++ x ++ ")"

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
            ("inr \"hi\"" `evaluatesTo` VInj True (VString "hi"))
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
            ("(\\x : int + bool + string. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inl 3)" `evaluatesTo` VInt 1)
        , testCase
            "nested 2"
            ("(\\x : int + bool + string. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inl false))" `evaluatesTo` VInt 2)
        , testCase
            "nested 2"
            ("(\\x : int + bool + string. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inr \"hi\"))" `evaluatesTo` VInt 3)
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
        "strings"
        [ testCase
            "format int"
            ("format 1" `evaluatesTo` VString "1")
        , testCase
            "format sum"
            ("format (inl 1)" `evaluatesTo` VString "inl 1")
        , testCase
            "format function"
            ("format (\\x. x + 1)" `evaluatesTo` VString "\\x. x + 1")
        , testCase
            "concat"
            ("\"x = \" ++ format (2+3) ++ \"!\"" `evaluatesTo` VString "x = 5!")
        ]
    ]
 where
  throwsError :: Text -> (Text -> Bool) -> Assertion
  throwsError tm p = do
    result <- evaluate tm
    case result of
      Right _ -> assertFailure "Unexpected success"
      Left err ->
        p err
          @? "Expected predicate did not hold on error message " ++ from @Text @String err

  evaluatesTo :: Text -> Value -> Assertion
  evaluatesTo tm val = do
    result <- evaluate tm
    assertEqual "" (Right val) (fst <$> result)

  evaluatesToP :: Text -> Value -> Property
  evaluatesToP tm val = ioProperty $ do
    result <- evaluate tm
    return $ Right val == (fst <$> result)

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

testModel :: TestTree
testModel =
  testGroup
    "TUI Model"
    [ testCase
        "latest repl lines at start"
        ( assertEqual
            "get 5 history [0] --> []"
            []
            (getLatestREPLHistoryItems 5 history0)
        )
    , testCase
        "latest repl lines after one input"
        ( assertEqual
            "get 5 history [0|()] --> [()]"
            [REPLEntry "()"]
            (getLatestREPLHistoryItems 5 (addREPLItem (REPLEntry "()") history0))
        )
    , testCase
        "latest repl lines after one input and output"
        ( assertEqual
            "get 5 history [0|1,1:int] --> [1,1:int]"
            [REPLEntry "1", REPLOutput "1:int"]
            (getLatestREPLHistoryItems 5 (addInOutInt 1 history0))
        )
    , testCase
        "latest repl lines after nine inputs and outputs"
        ( assertEqual
            "get 6 history [0|1,1:int .. 9,9:int] --> [7,7:int..9,9:int]"
            (concat [[REPLEntry (toT x), REPLOutput (toT x <> ":int")] | x <- [7 .. 9]])
            (getLatestREPLHistoryItems 6 (foldl (flip addInOutInt) history0 [1 .. 9]))
        )
    , testCase
        "latest repl after restart"
        ( assertEqual
            "get 5 history (restart [0|()]) --> []"
            []
            (getLatestREPLHistoryItems 5 (restartREPLHistory $ addREPLItem (REPLEntry "()") history0))
        )
    , testCase
        "current item at start"
        (assertEqual "getText [0] --> Nothing" (getCurrentItemText history0) Nothing)
    , testCase
        "current item after move to older"
        ( assertEqual
            "getText ([0]<=='') --> Just 0"
            (Just "0")
            (getCurrentItemText $ moveReplHistIndex Older "" history0)
        )
    , testCase
        "current item after move to newer"
        ( assertEqual
            "getText ([0]==>'') --> Nothing"
            Nothing
            (getCurrentItemText $ moveReplHistIndex Newer "" history0)
        )
    , testCase
        "current item after move past output"
        ( assertEqual
            "getText ([0,1,1:int]<=='') --> Just 1"
            (Just "1")
            (getCurrentItemText $ moveReplHistIndex Older "" (addInOutInt 1 history0))
        )
    , testCase
        "current item after move past same"
        ( assertEqual
            "getText ([0,1,1:int]<=='1') --> Just 0"
            (Just "0")
            (getCurrentItemText $ moveReplHistIndex Older "1" (addInOutInt 1 history0))
        )
    ]
 where
  history0 = newREPLHistory [REPLEntry "0"]
  toT :: Int -> Text
  toT = fromString . show
  addInOutInt :: Int -> REPLHistory -> REPLHistory
  addInOutInt i = addREPLItem (REPLOutput $ toT i <> ":int") . addREPLItem (REPLEntry $ toT i)

testInventory :: TestTree
testInventory =
  testGroup
    "Inventory"
    [ testCase
        "insert 0 / hash"
        ( assertEqual
            "insertCount 0 x empty has same hash as x"
            (x ^. E.entityHash)
            (hash (E.insertCount 0 x E.empty))
        )
    , testCase
        "insert / hash"
        ( assertEqual
            "insert x empty has same hash as 2*x"
            (2 * (x ^. E.entityHash))
            (hash (E.insert x E.empty))
        )
    , testCase
        "insert / insert"
        ( assertEqual
            "insert x y gives same hash as insert y x"
            (hash (E.insert x (E.insert y E.empty)))
            (hash (E.insert y (E.insert x E.empty)))
        )
    , testCase
        "insert 2 / delete"
        ( assertEqual
            "insert 2, delete 1 gives same hash as insert 1"
            (hash (E.insert x E.empty))
            (hash (E.delete x (E.insertCount 2 x E.empty)))
        )
    , testCase
        "insert 2 / delete 3"
        ( assertEqual
            "insert 2, delete 3 gives hash of x"
            (x ^. E.entityHash)
            (hash (E.deleteCount 3 x (E.insertCount 2 x E.empty)))
        )
    , testCase
        "deleteAll"
        ( assertEqual
            "insert 2 x, insert 2 y, deleteAll x same hash as insert 2 y, insertCount 0 x"
            (hash (E.insertCount 0 x (E.insertCount 2 y E.empty)))
            (hash (E.deleteAll x (E.insertCount 2 y (E.insertCount 2 x E.empty))))
        )
    , testCase
        "union"
        ( assertEqual
            "insert 2 x union insert 3 x same as insert 5 x"
            (hash (E.insertCount 5 x E.empty))
            (hash (E.union (E.insertCount 2 x E.empty) (E.insertCount 3 x E.empty)))
        )
    , testCase
        "difference"
        ( assertEqual
            "{(2,x),(3,y)} difference {(3,x),(1,y)} = {(0,x), (2,y)}"
            ( hash
                ( E.insertCount 2 x (E.insertCount 3 y E.empty)
                    `E.difference` E.insertCount 3 x (E.insertCount 1 y E.empty)
                )
            )
            (hash (E.insertCount 0 x (E.insertCount 2 y E.empty)))
        )
    , testCase
        "subset / yes"
        ( assertBool
            "{(0,x),(3,y),(2,z)} isSubsetOf {(3,y),(4,z)}"
            ( E.insertCount 0 x (E.insertCount 3 y (E.insertCount 2 z E.empty))
                `E.isSubsetOf` E.insertCount 3 y (E.insertCount 4 z E.empty)
            )
        )
    , testCase
        "subset / no"
        ( assertBool
            "{(2,x),(3,y)} isSubsetOf {(1,x),(4,y)}"
            ( not
                ( E.insertCount 2 x (E.insertCount 3 y E.empty)
                    `E.isSubsetOf` E.insertCount 1 x (E.insertCount 4 y E.empty)
                )
            )
        )
    , testCase
        "isEmpty / yes"
        ( assertBool
            "isEmpty {(0,x),(0,y)}"
            (E.isEmpty (E.insertCount 0 x (E.insertCount 0 y E.empty)))
        )
    , testCase
        "isEmpty / no"
        ( assertBool
            "isEmpty {(0,x),(1,y)}"
            (not (E.isEmpty (E.insertCount 0 x (E.insertCount 1 y E.empty))))
        )
    ]
 where
  x = E.mkEntity (defaultEntityDisplay 'X') "fooX" [] [] []
  y = E.mkEntity (defaultEntityDisplay 'Y') "fooY" [] [] []
  z = E.mkEntity (defaultEntityDisplay 'Z') "fooZ" [] [] []

testNotification :: GameState -> TestTree
testNotification gs =
  testGroup
    "Notifications"
    [ testCase "notifications at start" $ do
        assertNoNew gs "messages at game start" messageNotifications
        assertNoNew gs "recipes at game start" availableRecipes
        assertNoNew gs "commands at game start" availableCommands
    , testCase "new message after say" $ do
        (gs', _r, Right _valAndSteps) <- eval gs "say \"Hello world!\""
        assertNew gs' 1 "message" messageNotifications
    , testCase "two new messages after say twice" $ do
        (gs', _r, Right _valAndSteps) <- eval gs "say \"Hello!\"; say \"Goodbye!\""
        assertNew gs' 2 "message" messageNotifications
    , testCase "new message after log" $ do
        (gs', _r, Right _valAndSteps) <- eval gs "log \"Hello world!\""
        assertNew gs' 1 "message" messageNotifications
    , testCase "new message after build say" $ do
        (gs', _r, Right _valAndSteps) <- eval gs "build {say \"Hello world!\"}; turn back; turn back;"
        assertNew gs' 1 "message" messageNotifications
    , testCase "no new message after build log" $ do
        (gs', _r, Right _valAndSteps) <- eval gs "build {log \"Hello world!\"}; turn back; turn back;"
        assertNew gs' 1 "message" messageNotifications
    ]
 where
  assertNoNew :: s -> String -> Getter s (Notifications a) -> Assertion
  assertNoNew g what l =
    assertBool
      ("There should be no new " <> what)
      (g ^. l . notificationsCount == 0)
  assertNew :: s -> Int -> String -> Getter s (Notifications a) -> Assertion
  assertNew g n what l =
    assertBool
      ("There should be exactly " <> show n <> " new " <> what)
      (g ^. l . notificationsCount == n)

testMisc :: TestTree
testMisc =
  testGroup
    "Miscellaneous"
    [ testProperty
        "smallHittingSet produces hitting sets"
        (prop_hittingSet @QC.OrdA)
    ]

prop_hittingSet :: Ord a => [Set a] -> Property
prop_hittingSet ss = not (any S.null ss) ==> isHittingSet (smallHittingSet ss) ss

isHittingSet :: (Foldable t, Ord a) => Set a -> t (Set a) -> Bool
isHittingSet hs = not . any (S.null . S.intersection hs)

-- This property does *not* hold (and should not, because the problem
-- of producing a minimal hitting set is NP-hard), but we can use it
-- to generate counterexamples.
prop_hittingSetMinimal :: [Set El] -> Property
prop_hittingSetMinimal ss = not (any S.null ss) ==> all ((S.size hs <=) . S.size) allHittingSets
 where
  allElts = S.unions ss
  allSubsets = map S.fromList . subsequences . S.toList $ allElts
  allHittingSets = filter (`isHittingSet` ss) allSubsets
  hs = smallHittingSet ss

-- Five elements seem to be the minimum necessary for a
-- counterexample, but providing 6 helps QuickCheck find a
-- counterexample much more quickly.
data El = AA | BB | CC | DD | EE | FF
  deriving (Eq, Ord, Show, Bounded, Enum)

instance QC.Arbitrary El where
  arbitrary = QC.arbitraryBoundedEnum

-- ----------------------------------------------------------------------------
-- Utility functions
-- ----------------------------------------------------------------------------

eval :: GameState -> Text -> IO (GameState, Robot, Either Text (Value, Int))
eval g = either (return . (g,hypotheticalRobot undefined 0,) . Left) (evalPT g) . processTerm1

processTerm1 :: Text -> Either Text ProcessedTerm
processTerm1 txt = processTerm txt >>= maybe wsErr Right
 where
  wsErr = Left "expecting a term, but got only whitespace"

evalPT :: GameState -> ProcessedTerm -> IO (GameState, Robot, Either Text (Value, Int))
evalPT g t = evalCESK g (initMachine t empty emptyStore)

evalCESK :: GameState -> CESK -> IO (GameState, Robot, Either Text (Value, Int))
evalCESK g cesk = fmap orderResult . flip runStateT (g & creativeMode .~ True) . flip runStateT r . runCESK 0 $ cesk
 where
  r = hypotheticalRobot cesk 0
  orderResult ((res, rr), rg) = (rg, rr, res)

runCESK :: Int -> CESK -> StateT Robot (StateT GameState IO) (Either Text (Value, Int))
runCESK _ (Up exn _ []) = Left . flip formatExn exn <$> lift (use entityMap)
runCESK !steps cesk = case finalValue cesk of
  Just (v, _) -> return (Right (v, steps))
  Nothing -> stepCESK cesk >>= runCESK (steps + 1)
