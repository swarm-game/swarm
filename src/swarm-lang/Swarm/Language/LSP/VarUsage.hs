{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.LSP.VarUsage where

import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as J
import Swarm.Language.Parser.Util qualified as P
import Swarm.Language.Syntax
import Swarm.Util qualified as U

data BindingType
  = Lambda
  | Let
  | Bind
  deriving (Eq, Show)

data VarUsage = VarUsage LocVar BindingType

type BindingSites = Map Var (NonEmpty SrcLoc)

data Usage = Usage
  { usages :: Set LocVar
  -- ^ Variable references
  , problems :: [VarUsage]
  -- ^ Variable declarations without any references
  }

instance Semigroup Usage where
  Usage y1 z1 <> Usage y2 z2 =
    Usage
      (y1 <> y2)
      (z1 <> z2)

instance Monoid Usage where
  mempty = Usage mempty mempty

toErrPos :: Text -> VarUsage -> Maybe (J.Range, Text)
toErrPos code (VarUsage (LV loc v) scope) = do
  -- A leading underscore will suppress the unused variable warning
  guard $ not $ "_" `T.isPrefixOf` v
  rangePair <- case loc of
    SrcLoc s e -> Just (s, e)
    _ -> Nothing
  let (start, end) = P.getLocRange code rangePair
      ((startLine, startCol), (endLine, endCol)) = (minusOne start, minusOne end)
      range =
        J.Range
          (J.Position (fromIntegral startLine) (fromIntegral startCol))
          (J.Position (fromIntegral endLine) (fromIntegral endCol))
  return (range, txt)
 where
  txt =
    T.unwords
      [ "Unused variable"
      , U.quote v
      , "in"
      , T.pack $ show scope
      , "expression"
      ]
  minusOne (x, y) = (x - 1, y - 1)

-- | Descends the syntax tree rooted at a variable declaration,
-- accumulating variable references.
-- Generates a "problem" if an associated variable reference
-- is not encountered in the subtree for this declaration.
checkOccurrences ::
  BindingSites ->
  LocVar ->
  BindingType ->
  [Syntax] ->
  Usage
checkOccurrences bindings lv@(LV loc v) declType childSyntaxes =
  Usage childUsages $ missing <> deeperMissing
 where
  deeperBindings = M.insertWith (<>) v (pure loc) bindings
  Usage childUsages deeperMissing = mconcat $ map (getUsage deeperBindings) childSyntaxes
  missing = [VarUsage lv declType | lv `S.notMember` childUsages]

-- | Build up the bindings map as a function argument as
-- we descend into the syntax tree.
-- Aggregates unused bindings as we return from each layer.
getUsage ::
  BindingSites ->
  Syntax ->
  Usage
getUsage bindings (CSyntax _pos t _comments) = case t of
  TVar v -> Usage myUsages mempty
   where
    myUsages = case M.lookup v bindings of
      Nothing -> mempty
      Just (loc :| _) -> S.singleton $ LV loc v
  SLam v _ s -> checkOccurrences bindings v Lambda [s]
  SApp s1 s2 -> getUsage bindings s1 <> getUsage bindings s2
  -- Warn on unused 'let' bindings...
  SLet LSLet _ v _ _ _ s1 s2 -> getUsage bindings s1 <> checkOccurrences bindings v Let [s2]
  -- But don't warn on unused 'def' bindings, because they may be
  -- intended to be used at a later REPL input.
  SLet LSDef _ _ _ _ _ s1 s2 -> getUsage bindings s1 <> getUsage bindings s2
  SPair s1 s2 -> getUsage bindings s1 <> getUsage bindings s2
  SBind maybeVar _ _ _ s1 s2 -> case maybeVar of
    Just v -> checkOccurrences bindings v Bind [s1, s2]
    Nothing -> getUsage bindings s1 <> getUsage bindings s2
  SDelay s -> getUsage bindings s
  SRcd m -> M.foldMapWithKey (\x -> maybe (getUsage bindings (STerm (TVar x))) (getUsage bindings)) m
  SProj s _ -> getUsage bindings s
  SAnnotate s _ -> getUsage bindings s
  SSuspend s -> getUsage bindings s
  -- Explicitly enumerate the cases with no variables, instead of a
  -- catch-all, so that we get a warning when adding new constructors.
  TUnit {} -> mempty
  TConst {} -> mempty
  TDir {} -> mempty
  TInt {} -> mempty
  TAntiInt {} -> mempty
  TText {} -> mempty
  TAntiText {} -> mempty
  TBool {} -> mempty
  TRobot {} -> mempty
  TRef {} -> mempty
  TRequireDevice {} -> mempty
  TRequire {} -> mempty
  SRequirements {} -> mempty
  STydef {} -> mempty
  TType {} -> mempty
