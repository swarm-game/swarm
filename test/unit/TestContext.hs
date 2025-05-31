{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests for contexts
module TestContext where

import Control.Monad (replicateM)
import Data.Hashable
import Data.List (nub)
import Data.Map qualified as M
import Data.Yaml (decodeEither', encode)
import Swarm.Language.Context
import Swarm.Language.Var (Var)
import Swarm.Util (showT)
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, generate, testProperty, withMaxSuccess)

testContext :: TestTree
testContext =
  testGroup
    "Contexts"
    [ testGroup
        "Context equality"
        [ testCase "idempotence 1" $ ctxsEqual ctx1 (ctx1 <> ctx1)
        , testCase "idempotence 2" $ ctxsEqual ctx2 (ctx2 <> ctx2)
        , testCase "deletion" $ ctxsEqual ctx1 (delete "z" ctx2)
        , testCase "empty/delete" $ ctxsEqual empty (delete "x" ctx1)
        , testCase "fromMap" $ ctxsEqual ctx2 (fromMap (M.fromList [("x", 3), ("z", 6)]))
        , testCase "right bias" $ ctxsEqual ctx4 (ctx2 <> ctx3)
        , testCase "commutativity" $ ctxsEqual (ctx1 <> ctx5) (ctx5 <> ctx1)
        ]
    , testGroup
        "de/rehydrate round-trip"
        (map (\(name, ctx) -> testCase name $ hydrationRoundTrip ctx) testCtxs)
    , testGroup
        "serialize round-trip"
        (map (\(name, ctx) -> testCase name $ serializeRoundTrip ctx) testCtxs)
    , testProperty
        "no paired hash collisions"
        (withMaxSuccess 10000 (hashConsistent @Var @Int))
    , testCase
        "no hash collisions in a large pool"
        $ do
          ctxs <- generate (replicateM 100000 (arbitrary :: Gen (Ctx Var Int)))
          let m = M.fromListWith (++) (map (\ctx -> (ctxHash ctx, [unCtx ctx])) ctxs)
          assertBool "foo" $ all ((== 1) . length . nub) m
    ]
 where
  ctx1 = singleton "x" 3
  ctx2 = singleton "x" 3 <> singleton "z" 6
  ctx3 = singleton "x" 5 <> singleton "y" 7
  ctx4 = singleton "x" 5 <> singleton "y" 7 <> singleton "z" 6
  ctx5 = singleton "y" 10
  bigCtx = fromMap . M.fromList $ zip (map (("x" <>) . showT) [1 :: Int ..]) [1 .. 10000]

  testCtxs = [("empty", empty), ("ctx1", ctx1), ("ctx2", ctx2), ("ctx3", ctx3), ("ctx4", ctx4), ("ctx5", ctx5), ("large", bigCtx), ("delete", delete "y" ctx4)]

instance (Ord v, Hashable v, Arbitrary v, Hashable a, Arbitrary a) => Arbitrary (Ctx v a) where
  arbitrary = fromMap <$> arbitrary

hashConsistent :: (Eq v, Eq a) => Ctx v a -> Ctx v a -> Bool
hashConsistent ctx1 ctx2 = (ctx1 == ctx2) == (ctx1 `ctxStructEqual` ctx2)

ctxsEqual :: Ctx Var Int -> Ctx Var Int -> Assertion
ctxsEqual ctx1 ctx2 = do
  -- Contexts are compared by hash for equality
  assertEqual "hash equality" ctx1 ctx2

  -- Make sure they are also structurally equal
  assertBool "structural equality" (ctxStructEqual ctx1 ctx2)

ctxStructEqual :: (Eq a, Eq v) => Ctx v a -> Ctx v a -> Bool
ctxStructEqual (Ctx m1 _) (Ctx m2 _) = m1 == m2

hydrationRoundTrip :: Ctx Var Int -> Assertion
hydrationRoundTrip ctx = do
  case getCtx (ctxHash ctx) (rehydrate (dehydrate (toCtxMap ctx))) of
    Nothing -> fail "Failed to reconstitute dehydrated context"
    Just ctx' -> ctxsEqual ctx ctx'

serializeRoundTrip :: Ctx Var Int -> Assertion
serializeRoundTrip ctx = do
  case decodeEither' (encode (dehydrate (toCtxMap ctx))) of
    Left e -> fail $ "Failed to decode JSON-encoded context: " ++ show e
    Right c -> case getCtx (ctxHash ctx) (rehydrate c) of
      Nothing -> fail "Failed to reconstitute dehydrated context"
      Just ctx' -> ctxsEqual ctx ctx'
