{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm unit tests
module Main where

import Control.Lens ((&), (.~))
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Linear
import Test.Tasty
import Test.Tasty.HUnit
import Witch (from)

import Swarm.Game.CESK
import Swarm.Game.Exception
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Step
import Swarm.Game.Value
import Swarm.Language.Context
import Swarm.Language.Pipeline (ProcessedTerm (..), processTerm)
import Swarm.Language.Pretty
import Swarm.Language.Syntax hiding (mkOp)

main :: IO ()
main = do
  mg <- runExceptT (initGameState 0)
  case mg of
    Left err -> assertFailure (from err)
    Right g -> defaultMain (tests g)

tests :: GameState -> TestTree
tests g = testGroup "Tests" [parser, prettyConst, eval g]

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
    , testCase
        "parsing operators #188 - parse valid operator (!=)"
        (valid "1!=(2)")
    , testCase
        "parsing operators #236 - parse valid operator (<=)"
        (valid "1 <= 2")
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
    , testCase
        "parsing operators #??? - parse valid operator ($)"
        (valid "fst $ snd $ (1,2,3)")
    ]
 where
  valid = flip process ""

  process :: Text -> Text -> Assertion
  process code expect = case processTerm code of
    Left e
      | not (T.null expect) && expect `T.isPrefixOf` e -> pure ()
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

eval :: GameState -> TestTree
eval g =
  testGroup
    "Language - evaluation"
    [ testGroup
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
            "raise"
            ("raise \"foo\"" `throwsError` ("foo" `T.isInfixOf`))
        , testCase
            "try / no exception 1"
            ("try {return 1} {return 2}" `evaluatesTo` VInt 1)
        , testCase
            "try / no exception 2"
            ("try {return 1} {let x = x in x}" `evaluatesTo` VInt 1)
        , testCase
            "try / raise"
            ("try {raise \"foo\"} {return 3}" `evaluatesTo` VInt 3)
        , testCase
            "try / raise / raise"
            ("try {raise \"foo\"} {raise \"bar\"}" `throwsError` ("bar" `T.isInfixOf`))
        , testCase
            "try / div by 0"
            ("try {return (1/0)} {return 3}" `evaluatesTo` VInt 3)
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
    assertEqual "" (Right val) result

  evaluate :: Text -> IO (Either Text Value)
  evaluate = either (return . Left) evalPT . processTerm

  evalPT :: ProcessedTerm -> IO (Either Text Value)
  evalPT t = evaluateCESK (initMachine t empty emptyStore)

  evaluateCESK :: CESK -> IO (Either Text Value)
  evaluateCESK cesk = flip evalStateT (g & gameMode .~ Creative) . flip evalStateT r . runCESK $ cesk
   where
    r = mkRobot "" zero zero cesk []

  runCESK :: CESK -> StateT Robot (StateT GameState IO) (Either Text Value)
  runCESK (Up exn _ []) = return (Left (formatExn exn))
  runCESK cesk = case finalValue cesk of
    Just (v, _) -> return (Right v)
    Nothing -> stepCESK cesk >>= runCESK
