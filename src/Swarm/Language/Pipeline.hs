{-# LANGUAGE TypeOperators #-}

module Swarm.Language.Pipeline where

import           Data.Bifunctor        (first)
import qualified Data.Map              as M
import           Data.Text             (Text)

import           Swarm.Language.Elaborate
import           Swarm.Language.Syntax
import           Swarm.Language.Types
import           Swarm.Language.Parse
import           Swarm.Language.Pretty
import           Swarm.Language.Typecheck


processCmd :: Text -> Either Text ATerm
processCmd txt = do
  t <- readTerm txt
  at <- first prettyText (check M.empty t (TyCmd TyUnit))
  return $ elaborate (TyCmd TyUnit) at

processTerm :: Text -> Either Text (ATerm ::: Type)
processTerm txt = do
  t <- readTerm txt
  at ::: ty <- first prettyText (infer M.empty t)
  return $ elaborate ty at ::: ty
