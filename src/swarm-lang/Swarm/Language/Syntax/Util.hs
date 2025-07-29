{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Helper functions for working with @Terms@ and @Syntax@
module Swarm.Language.Syntax.Util (
  -- * Utility AST construction/destruction
  mkOp,
  mkOp',
  unfoldApps,
  mkTuple,
  unTuple,
  locVarToSyntax,

  -- * Traversals

  -- ** Term + type traversal
  termSyntax,
  traverseSyntax,

  -- ** Erasure
  Erasable (..),

  -- ** Free variable traversal
  freeVarsS,
  freeVarsT,
  freeVarsV,
  mapFreeS,

  -- ** Miscellaneous traversals
  asTree,
  measureAstSize,
) where

import Control.Lens (Traversal', para, universe, (%~), (^.), pattern Empty)
import Data.Data (Data, Typeable)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as S
import Data.Tree
import Swarm.Language.Phase
import Swarm.Language.Syntax.AST
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Import (Anchor, ImportLoc, Unresolvable, unresolveImportLoc)
import Swarm.Language.Syntax.Loc
import Swarm.Language.Syntax.Pattern
import Swarm.Language.Var (LocVar, Var)

-- Setup for doctests

-- $setup
-- >>> import Control.Lens ((^.))
-- >>> import Swarm.Language.Syntax.Constants
-- >>> import Swarm.Util.SrcLoc
-- >>> import Swarm.Language.Syntax.Pattern
-- >>> import Swarm.Language.Syntax.AST

-- | Make an infix operation (e.g. @2 + 3@) a curried function
--   application (e.g. @((+) 2) 3@).
mkOp :: Const -> (SrcLoc, t) -> Syntax Raw -> Syntax Raw -> Syntax Raw
mkOp c (opLoc, _) s1@(RSyntax l1 _) s2@(RSyntax l2 _) = RSyntax newLoc newTerm
 where
  -- The new syntax spans all terms
  newLoc = l1 <> opLoc <> l2
  sop = RSyntax opLoc (TConst c)
  newTerm = SApp (RSyntax (l1 <> opLoc) $ SApp sop s1) s2

-- | Make an infix operation, discarding any location information
mkOp' :: Const -> Term Raw -> Term Raw -> Term Raw
mkOp' c t1 = TApp (TApp (TConst c) t1)

-- | Turn function application chain into a list.
--
-- >>> syntaxWrap f = fmap (^. sTerm) . f . RSyntax NoLoc
-- >>> syntaxWrap unfoldApps (mkOp' Mul (TInt 1) (TInt 2)) -- 1 * 2
-- TConst Mul :| [TInt 1,TInt 2]
unfoldApps :: Syntax phase -> NonEmpty (Syntax phase)
unfoldApps trm = NonEmpty.reverse . flip NonEmpty.unfoldr trm $ \case
  Syntax _ (SApp s1 s2) _ _ -> (s2, Just s1)
  s -> (s, Nothing)

-- | Create an appropriate `Term Raw` out of a list of syntax nodes which
--   were enclosed with parentheses (and separated by commas).
mkTuple :: [Syntax Raw] -> Term Raw
-- () = TUnit
mkTuple [] = TUnit
-- (x) = x, but record the fact that it was explicitly parenthesized,
-- for better source location tracking
mkTuple [x] = SParens x
-- (x,y) = SPair
mkTuple [x, y] = SPair x y
-- (x,y,...) = recursively nested pairs.  Note that we do not assign
-- source spans to the nested tuples since they don't really come from
-- a specific place in the source.
mkTuple (x : r) = SPair x (RSyntax NoLoc (mkTuple r))

-- | Decompose a nested tuple into a list of components.
unTuple :: Syntax phase -> [Syntax phase]
unTuple = \case
  Syntax _ (SPair s1 s2) _ _ -> s1 : unTuple s2
  s -> [s]

locVarToSyntax :: LocVar -> SwarmType phase -> Syntax phase
locVarToSyntax (Loc s v) = Syntax s (TVar v) Empty

------------------------------------------------------------
-- Term + type traversal
------------------------------------------------------------

-- | Traversal to pick out all Syntax nodes and types inside a Term.
--   Generic over the phase so we can use it to change phase.
--
--   This would in fact be some kind of 'Tritraversal' (if there were
--   such a thing defined in the lens package).
termSyntax ::
  Applicative f =>
  (SwarmType a -> f (SwarmType b)) ->
  (ImportLoc (ImportPhaseFor a) -> f (ImportLoc (ImportPhaseFor b))) ->
  (Syntax a -> f (Syntax b)) ->
  Term a ->
  f (Term b)
termSyntax fty floc fsyn = \case
  TUnit -> pure TUnit
  TConst c -> pure $ TConst c
  TDir d -> pure $ TDir d
  TInt i -> pure $ TInt i
  TAntiInt t -> pure $ TAntiInt t
  TText t -> pure $ TText t
  TAntiText t -> pure $ TAntiText t
  TBool b -> pure $ TBool b
  TAntiSyn t -> pure $ TAntiSyn t
  TRobot r -> pure $ TRobot r
  TRef x -> pure $ TRef x
  TRequire d -> pure $ TRequire d
  TStock n d -> pure $ TStock n d
  SRequirements t s -> SRequirements t <$> fsyn s
  TVar x -> pure $ TVar x
  SPair s1 s2 -> SPair <$> fsyn s1 <*> fsyn s2
  SLam x ty s -> SLam x ty <$> fsyn s
  SApp s1 s2 -> SApp <$> fsyn s1 <*> fsyn s2
  SLet ls r x rty ty req s1 s2 -> SLet ls r x rty ty req <$> fsyn s1 <*> fsyn s2
  STydef x ty info s -> STydef x ty info <$> fsyn s
  SBind x t1 t2 req s1 s2 -> SBind x <$> traverse fty t1 <*> pure t2 <*> pure req <*> fsyn s1 <*> fsyn s2
  SDelay s -> SDelay <$> fsyn s
  SRcd m -> SRcd <$> (traverse . traverse . traverse) fsyn m
  SProj s x -> SProj <$> fsyn s <*> pure x
  SAnnotate s pty -> SAnnotate <$> fsyn s <*> pure pty
  SSuspend s -> SSuspend <$> fsyn s
  SParens s -> SParens <$> fsyn s
  TType ty -> pure $ TType ty
  SImportIn loc s -> SImportIn <$> floc loc <*> fsyn s

-- | Given a (possibly effectful) way to turn types from one phase
--   into types at another phase, and likewise a way to transform
--   imports, map over all types in a syntax tree, effectfully
--   changing the phase of the syntax tree as a whole.
--
--   We could make this a @Traversal (Syntax a) (Syntax b) (SwarmType
--   a) (SwarmType b)@ but we just keep an explicit type like this for
--   simplicity.
traverseSyntax ::
  Applicative f =>
  (SwarmType a -> f (SwarmType b)) ->
  (ImportLoc (ImportPhaseFor a) -> f (ImportLoc (ImportPhaseFor b))) ->
  Syntax a ->
  f (Syntax b)
traverseSyntax f g (Syntax loc t com ty) =
  Syntax loc <$> termSyntax f g (traverseSyntax f g) t <*> pure com <*> f ty

------------------------------------------------------------
-- Type erasure
------------------------------------------------------------

-- | Erase type annotations.
class Erasable t where
  erase :: t Elaborated -> t Resolved
  eraseRaw :: Unresolvable (ImportPhaseFor phase) => t phase -> t Raw

instance Erasable Syntax where
  erase = runIdentity . traverseSyntax (const (pure ())) pure
  eraseRaw = runIdentity . traverseSyntax (const (pure ())) (pure . unresolveImportLoc)

instance Erasable Term where
  erase = runIdentity . termSyntax (const (pure ())) pure (pure . erase)
  eraseRaw = runIdentity . termSyntax (const (pure ())) (pure . unresolveImportLoc) (pure . eraseRaw)

------------------------------------------------------------
-- Free variable traversals
------------------------------------------------------------

-- | Traversal over those subterms of a term which represent free
--   variables.  The S suffix indicates that it is a `Traversal' over
--   the `Syntax` nodes (which contain type and source location info)
--   containing free variables inside a larger `Syntax` value.  Note
--   that if you want to get the list of all `Syntax` nodes
--   representing free variables, you can do so via @'toListOf'
--   'freeVarsS'@.
freeVarsS :: forall phase. Traversal' (Syntax phase) (Syntax phase)
freeVarsS f = go S.empty
 where
  -- go :: Applicative f => Set Var -> Syntax phase -> f (Syntax phase)
  go bound s@(Syntax l t ty cmts) = case t of
    TUnit -> pure s
    TConst {} -> pure s
    TDir {} -> pure s
    TInt {} -> pure s
    TAntiInt {} -> pure s
    TText {} -> pure s
    TAntiText {} -> pure s
    TAntiSyn {} -> pure s
    TBool {} -> pure s
    TRobot {} -> pure s
    TRef {} -> pure s
    TRequire {} -> pure s
    TStock {} -> pure s
    SRequirements x s1 -> rewrap $ SRequirements x <$> go bound s1
    TVar x
      | x `S.member` bound -> pure s
      | otherwise -> f s
    SLam x xty s1 -> rewrap $ SLam x xty <$> go (S.insert (locVal x) bound) s1
    SApp s1 s2 -> rewrap $ SApp <$> go bound s1 <*> go bound s2
    SLet ls r x xty xpty xreq s1 s2 ->
      let bound' = S.insert (locVal x) bound
       in rewrap $ SLet ls r x xty xpty xreq <$> go bound' s1 <*> go bound' s2
    STydef x xdef tdInfo t1 -> rewrap $ STydef x xdef tdInfo <$> go bound t1
    SPair s1 s2 -> rewrap $ SPair <$> go bound s1 <*> go bound s2
    SBind mx mty mpty mreq s1 s2 -> rewrap $ SBind mx mty mpty mreq <$> go bound s1 <*> go (maybe id (S.insert . locVal) mx bound) s2
    SDelay s1 -> rewrap $ SDelay <$> go bound s1
    SRcd m -> rewrap $ SRcd <$> (traverse . traverse . traverse) (go bound) m
    SProj s1 x -> rewrap $ SProj <$> go bound s1 <*> pure x
    SAnnotate s1 pty -> rewrap $ SAnnotate <$> go bound s1 <*> pure pty
    SSuspend s1 -> rewrap $ SSuspend <$> go bound s1
    SParens s1 -> rewrap $ SParens <$> go bound s1
    TType {} -> pure s
    SImportIn url s1 -> rewrap $ SImportIn url <$> go bound s1
   where
    rewrap s' = Syntax l <$> s' <*> pure ty <*> pure cmts

-- | Like 'freeVarsS', but traverse over the 'Term's containing free
--   variables.  More direct if you don't need to know the types or
--   source locations of the variables.  Note that if you want to get
--   the list of all `Term`s representing free variables, you can do
--   so via @'toListOf' 'freeVarsT'@.
freeVarsT :: forall phase. Traversal' (Syntax phase) (Term phase)
freeVarsT = freeVarsS . sTerm

-- | Traversal over the free variables of a term.  Like 'freeVarsS'
--   and 'freeVarsT', but traverse over the variable names themselves.
--   Note that if you want to get the set of all free variable names,
--   you can do so via @'Data.Set.Lens.setOf' 'freeVarsV'@.
freeVarsV :: Traversal' (Syntax phase) Var
freeVarsV = freeVarsT . (\f -> \case TVar x -> TVar <$> f x; t -> pure t)

-- | Apply a function to all free occurrences of a particular
--   variable.
mapFreeS :: Var -> (Syntax phase -> Syntax phase) -> Syntax phase -> Syntax phase
mapFreeS x f = freeVarsS %~ (\t -> case t ^. sTerm of TVar y | y == x -> f t; _ -> t)

------------------------------------------------------------
-- Other traversals
------------------------------------------------------------

-- | Transform the AST into a Tree datatype.  Useful for
--   pretty-printing (e.g. via "Data.Tree.drawTree").
asTree :: (Data (Anchor (ImportPhaseFor phase)), Typeable phase, Typeable (ImportPhaseFor phase), Data (SwarmType phase)) => Syntax phase -> Tree (Syntax phase)
asTree = para Node

-- | Each constructor is a assigned a value of 1, plus
--   any recursive syntax it entails.
measureAstSize :: (Data (Anchor (ImportPhaseFor phase)), Typeable phase, Typeable (ImportPhaseFor phase), Data (SwarmType phase)) => Syntax phase -> Int
measureAstSize = length . filter (not . isNoop) . universe

-- | Don't count "noop" nodes towards the code size.  They are usually
--   inserted automatically, either in @{}@ or after a bare @def@.
isNoop :: Syntax a -> Bool
isNoop = \case
  Syntax _ (TConst Noop) _ _ -> True
  _ -> False
