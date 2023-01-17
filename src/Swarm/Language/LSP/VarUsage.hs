{-# LANGUAGE OverloadedStrings #-}

module Swarm.Language.LSP.VarUsage where

import Control.Monad (guard)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Types qualified as J
import Swarm.Language.Parse qualified as P
import Swarm.Language.Syntax
import Swarm.Util qualified as U

data BindingType
  = Lambda
  | Let
  | Bind
  deriving (Eq, Show)

data VarUsage = VarUsage LocVar BindingType

type BindingSites = Map Var (BindingType, Syntax)

data Usage = Usage
  { usages :: Set Var
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
  Map Var (BindingType, Syntax) ->
  Syntax ->
  LocVar ->
  BindingType ->
  [Syntax] ->
  Usage
checkOccurrences bindingSites s lv@(LV _ v) declType childSyntaxes =
  Usage childUsages $ missing <> childMissing
 where
  -- NOTE: "union" and "mappend" are left-biased, so to overwrite,
  -- we put the new bindings on the left and the inherited
  -- bindings on the right.
  bindingsForChildren = M.singleton v (declType, s) <> bindingSites
  Usage childUsages childMissing = mconcat $ map (getUsage bindingsForChildren) childSyntaxes

  missing = [VarUsage lv declType | v `S.notMember` childUsages]

-- | Build up the bindings map as a function argument as
-- we descend into the syntax tree.
-- Aggregates unused bindings as we return from each layer.
getUsage ::
  BindingSites ->
  Syntax ->
  Usage
getUsage bindingSites s0@(Syntax _pos t) = case t of
  TVar v -> Usage myUsages mempty
   where
    myUsages = case M.lookup v bindingSites of
      Nothing -> mempty
      Just _ -> S.singleton v
  SLam v _ s -> checkOccurrences bindingSites s0 v Lambda [s]
  SApp s1 s2 -> getUsage bindingSites s1 <> getUsage bindingSites s2
  SLet _ v _ s1 s2 -> checkOccurrences bindingSites s0 v Let [s1, s2]
  SPair s1 s2 -> getUsage bindingSites s1 <> getUsage bindingSites s2
  SDef _ _v _ s -> getUsage bindingSites s
  SBind maybeVar s1 s2 -> case maybeVar of
    Just v -> checkOccurrences bindingSites s0 v Bind [s1, s2]
    Nothing -> getUsage bindingSites s1 <> getUsage bindingSites s2
  SDelay _ s -> getUsage bindingSites s
  _ -> mempty
