{-# LANGUAGE PatternSynonyms #-}

module Swarm.Language.Syntax.Parsed where

import Swarm.Language.Syntax

type instance Annot Parsed = ()

noLoc :: Term Parsed -> Syntax Parsed
noLoc t = Syntax mempty t ()

-- | Match an untyped term without its 'SrcLoc'.
pattern STerm :: Term Parsed -> Syntax Parsed
pattern STerm t <-
  Syntax _ t ()
  where
    STerm t = Syntax mempty t ()

pattern TRequirements :: Text -> Term Parsed -> Term Parsed
pattern TRequirements x t = SRequirements x (STerm t)

-- | Match a TPair without syntax
pattern TPair :: Term Parsed -> Term Parsed -> Term Parsed
pattern TPair t1 t2 = SPair (STerm t1) (STerm t2)

-- | Match a TLam without syntax
pattern TLam :: Var -> Maybe Type -> Term Parsed -> Term Parsed
pattern TLam v ty t <- SLam (lvVar -> v) ty (STerm t)
  where
    TLam v ty t = SLam (LV NoLoc v) ty (STerm t)

-- | Match a TApp without syntax
pattern TApp :: Term Parsed -> Term Parsed -> Term Parsed
pattern TApp t1 t2 = SApp (STerm t1) (STerm t2)

infixl 0 :$:

-- | Convenient infix pattern synonym for application.
pattern (:$:) :: Term Parsed -> Syntax Parsed -> Term Parsed
pattern (:$:) t1 s2 = SApp (STerm t1) s2

-- | Match a TLet without syntax
pattern TLet :: Bool -> Var -> Maybe Polytype -> Term Parsed -> Term Parsed -> Term Parsed
pattern TLet r v pt t1 t2 <- SLet r (lvVar -> v) pt (STerm t1) (STerm t2)
  where
    TLet r v pt t1 t2 = SLet r (LV NoLoc v) pt (STerm t1) (STerm t2)

-- | Match a TDef without syntax
pattern TDef :: Bool -> Var -> Maybe Polytype -> Term Parsed -> Term Parsed
pattern TDef r v pt t <- SDef r (lvVar -> v) pt (STerm t)
  where
    TDef r v pt t = SDef r (LV NoLoc v) pt (STerm t)

-- | Match a TBind without syntax
pattern TBind :: Maybe Var -> Term Parsed -> Term Parsed -> Term Parsed
pattern TBind mv t1 t2 <- SBind (fmap lvVar -> mv) (STerm t1) (STerm t2)
  where
    TBind mv t1 t2 = SBind (LV NoLoc <$> mv) (STerm t1) (STerm t2)

-- | Match a TDelay without syntax
pattern TDelay :: DelayType -> Term Parsed -> Term Parsed
pattern TDelay m t = SDelay m (STerm t)

-- | Match a TRcd without syntax
pattern TRcd :: Map Var (Maybe (Term Parsed)) -> Term Parsed
pattern TRcd m <- SRcd ((fmap . fmap) _sTerm -> m)
  where
    TRcd m = SRcd ((fmap . fmap) STerm m)

pattern TProj :: Term Parsed -> Var -> Term Parsed
pattern TProj t x = SProj (STerm t) x

-- | Match a TAnnotate without syntax
pattern TAnnotate :: Term Parsed -> Polytype -> Term Parsed
pattern TAnnotate t pt = SAnnotate (STerm t) pt

-- | COMPLETE pragma tells GHC using this set of pattern is complete for Term
{-# COMPLETE TUnit, TConst, TDir, TInt, TAntiInt, TText, TAntiText, TBool, TRequireDevice, TRequire, TRequirements, TVar, TPair, TLam, TApp, TLet, TDef, TBind, TDelay, TRcd, TProj, TAnnotate #-}

-- | Make infix operation (e.g. @2 + 3@) a curried function
--   application (@((+) 2) 3@).
mkOp :: (() ~ Annot Parsed) => Const -> Syntax Parsed -> Syntax Parsed -> Syntax Parsed
mkOp c s1@(Syntax l1 _ ()) s2@(Syntax l2 _ ()) = Syntax newLoc newTerm ()
 where
  -- The new syntax span both terms
  newLoc = l1 <> l2
  -- We don't assign a source location for the operator since it is
  -- usually provided as-is and it is not likely to be useful.
  sop = noLoc (TConst c)
  newTerm = SApp (Syntax l1 (SApp sop s1) ()) s2

-- | Make infix operation, discarding any syntax related location
mkOp' :: Const -> Term Parsed -> Term Parsed -> Term Parsed
mkOp' c t1 = TApp (TApp (TConst c) t1)
