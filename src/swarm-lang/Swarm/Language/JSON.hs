{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Some orphan @To/FromJSON@ instances for terms and values.  We have
-- to put them all here to avoid circular module dependencies.
module Swarm.Language.JSON where

import Data.Aeson (FromJSON (..), ToJSON (..), genericToJSON, withText)
import Data.Aeson qualified as Ae
import Swarm.Language.Pipeline (processTermEither)
import Swarm.Language.Syntax (Term)
import Swarm.Language.Syntax.Pattern (Syntax, TSyntax)
import Swarm.Language.Value (Env, Value (..))
import Swarm.Pretty (prettyText)
import Swarm.Util.JSON (optionsMinimize)
import Witch (into)

instance FromJSON TSyntax where
  parseJSON = withText "Term" $ either (fail . into @String) return . processTermEither

instance ToJSON TSyntax where
  toJSON = Ae.String . prettyText

instance FromJSON Term
instance FromJSON Syntax
instance ToJSON Term
instance ToJSON Syntax

instance ToJSON Value where
  toJSON = genericToJSON optionsMinimize

-- TODO (#2213): Craft some appropriate FromJSONE instances for things
-- like Value and Env.  Below is an early experiment.

-- instance FromJSONE (CtxMap CtxTree t) Value where
--   parseJSONE = withObjectE "Value" $ \v -> case Ae.toList v of
--     [("VUnit", _)] -> pure VUnit
--     [("VInt", n)] -> VInt <$> liftE (parseJSON n)
--     [("VText", t)] -> VText <$> liftE (parseJSON t)
--     [("VInj", Ae.Array (V.toList -> [i, x]))] -> VInj <$> liftE (parseJSON i) <*> parseJSONE x
--     [("VPair", Ae.Array (V.toList -> [v1, v2]))] -> VPair <$> parseJSONE v1 <*> parseJSONE v2
--     [("VClo", Ae.Array (V.toList -> [x, t, e]))] ->
--       VClo <$> liftE (parseJSON x) <*> liftE (parseJSON t) <*> parseJSONE e
--     [("VCApp", Ae.Array (V.toList -> [c, vs]))] ->
--       VCApp <$> liftE (parseJSON c) <*> parseJSONE vs
--     [("VBind", Ae.Array (V.toList -> [x, ty, r, t1, t2, e]))] ->
--       VBind
--         <$> liftE (parseJSON x)
--         <*> liftE (parseJSON ty)
--         <*> liftE (parseJSON r)
--         <*> liftE (parseJSON t1)
--         <*> liftE (parseJSON t2)
--         <*> parseJSONE e
--     [("VDelay", Ae.Array (V.toList -> [t, e]))] ->
--       VDelay <$> liftE (parseJSON t) <*> parseJSONE e
--     [("VRef", n)] -> VRef <$> liftE (parseJSON n)
--     [("VIndir", n)] -> VIndir <$> liftE (parseJSON n)
--     [("VRcd", m)] -> VRcd <$> parseJSONE m
--     [("VKey", k)] -> VKey <$> liftE (parseJSON k)
--     [("VRequirements", Ae.Array (V.toList -> [txt, t, e]))] ->
--       VRequirements <$> liftE (parseJSON txt) <*> liftE (parseJSON t) <*> parseJSONE e
--     [("VSuspend", Ae.Array (V.toList -> [t, e]))] ->
--       VSuspend <$> liftE (parseJSON t) <*> parseJSONE e
--     [("VExc", _)] -> pure VExc
--     [("VBlackhole", _)] -> pure VBlackhole
--     _ -> fail "parseJSONE: Unable to parse Value"

instance ToJSON Env where
  toJSON = genericToJSON optionsMinimize
