{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Kind checking + type name resolution for the Swarm language.
module Swarm.Language.Kindcheck (
  KindError (..),
  processPolytype,
  processType,
) where

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Throw (Throw, throwError)
import Control.Monad.Extra (unlessM)
import Data.Fix (Fix (..))
import Prettyprinter (hsep, nest, pretty, vsep, (<+>))
import Swarm.Language.Types
import Swarm.Pretty (PrettyPrec (..), ppr)
import Swarm.Util (number)
import Swarm.Util.Effect (withThrow)

------------------------------------------------------------
-- Type processing

-- | Process a polytype, by doing name resolution and kind checking,
--   and returning an appropriate 'TydefInfo' record to be used in the
--   case of a type definition.
processPolytype :: (Has (Reader TDCtx) sig m, Has (Throw KindError) sig m) => Polytype -> m TydefInfo
processPolytype pty@(unPoly -> (xs, _)) = do
  pty' <- traverse processType pty
  pure $ TydefInfo pty' (Arity $ length xs)

-- | Process a type by doing name resolution and kind checking.
processType :: (Has (Reader TDCtx) sig m, Has (Throw KindError) sig m) => Type -> m Type
processType ty = do
  ty' <- resolveTydefs ty
  checkKind ty'
  pure ty'

------------------------------------------------------------
-- Tydef name resolution

-- | Name resolution for user-defined type names: for each
--   user-defined type name found anywhere in the type, resolve it to
--   the correct version number depending on what is in scope, by
--   calling 'resolveUserTy'.
resolveTydefs :: Has (Reader TDCtx) sig m => Type -> m Type
resolveTydefs (Fix tyF) =
  Fix <$> case tyF of
    TyConF tc tys -> do
      tc' <- case tc of
        TCUser u -> TCUser <$> resolveUserTy u
        _ -> pure tc
      TyConF tc' <$> mapM resolveTydefs tys
    TyRcdF m -> TyRcdF <$> mapM resolveTydefs m
    TyRecF x t -> TyRecF x <$> resolveTydefs t
    TyVarF {} -> pure tyF
    TyRecVarF {} -> pure tyF

------------------------------------------------------------
-- Kind checking

-- | Kind checking errors that can occur.
data KindError
  = -- | A type constructor expects n arguments, but was given these
    --   arguments instead.
    ArityMismatch TyCon Int [Type]
  | -- | An undefined type constructor was encountered in the given type.
    UndefinedTyCon TyCon Type
  | -- | A trivial recursive type (one that does not use its bound
    --   variable) was encountered.
    TrivialRecTy Var Type
  | -- | A vacuous recursive type (one that expands immediately to
    --   itself) was encountered.
    VacuousRecTy Var Type
  deriving (Eq, Show)

instance PrettyPrec KindError where
  prettyPrec _ = \case
    ArityMismatch c a tys ->
      nest 2 . vsep $
        [ "Kind error:"
        , hsep
            [ ppr c
            , "requires"
            , pretty a
            , "type"
            , pretty (number a "argument" <> ",")
            , "but was given"
            , pretty (length tys)
            ]
        ]
          ++ ["in the type:" <+> ppr (TyConApp c tys) | not (null tys)]
    UndefinedTyCon tc _ty -> "Undefined type" <+> ppr tc
    TrivialRecTy x ty ->
      nest 2 . vsep $
        [ "Encountered trivial recursive type" <+> ppr (TyRec x ty)
        , "Did you forget to use" <+> ppr x <+> "in the body of the type?"
        ]
    VacuousRecTy x ty ->
      nest 2 . vsep $
        [ "Encountered vacuous recursive type" <+> ppr (TyRec x ty)
        , "Recursive types must be productive, i.e. must not expand to themselves."
        ]

-- | Check that a type is well-kinded. For now, we don't allow
--   higher-kinded types, *i.e.* all kinds will be of the form @Type
--   -> Type -> ... -> Type@ which can be represented by a number (the
--   arity); every type constructor must also be fully applied. So, we
--   only have to check that each type constructor is applied to the
--   correct number of type arguments.  In the future, we might very
--   well want to generalize to arbitrary higher kinds (e.g. @(Type ->
--   Type) -> Type@ etc.) which would require generalizing this
--   checking code a bit.
--
--   Here we also check that any recursive types are non-vacuous,
--   /i.e./ not of the form @rec t. t@, and non-trivial, /i.e./ the
--   variable bound by the @rec@ actually occurs somewhere in the
--   body.
checkKind :: (Has (Reader TDCtx) sig m, Has (Throw KindError) sig m) => Type -> m ()
checkKind ty@(Fix tyF) = case tyF of
  TyConF c tys -> do
    tdCtx <- ask
    case getArity <$> tcArity tdCtx c of
      Nothing -> throwError $ UndefinedTyCon c ty
      Just a -> case compare (length tys) a of
        EQ -> mapM_ checkKind tys
        _ -> throwError $ ArityMismatch c a tys
  TyVarF _ _ -> return ()
  TyRcdF m -> mapM_ checkKind m
  TyRecF x t -> do
    -- It's important to call checkKind first, to rule out undefined
    -- type constructors. Within the recursive kind check, we
    -- substitute the given variable name for the bound de Bruijn
    -- index 0 in the body.  This doesn't affect the checking but it
    -- does ensure that error messages will use the variable name and
    -- not de Bruijn indices.
    checkKind (substRec (TyVarF x x) t NZ)
    -- Now check that the recursive type is well-formed.  We call this
    -- with the *unsubstituted* t because the check will be looking
    -- for de Bruijn variables specifically.
    checkRecTy x t
  TyRecVarF _ -> return ()

-- | Check that the body of a recursive type actually contains the
--   bound variable at least once (otherwise there's no point in using
--   @rec@) and does not consist solely of that variable.
checkRecTy :: (Has (Reader TDCtx) sig m, Has (Throw KindError) sig m) => Var -> Type -> m ()
checkRecTy x ty = do
  unlessM (containsVar NZ ty) $ throwError (TrivialRecTy x ty)
  unlessM (nonVacuous NZ ty) $ throwError (VacuousRecTy x ty)

-- Note, in theory it would be more efficient to combine containsVar
-- and nonVacuous into a single check that walks over the type only
-- once, but we keep them separate just to simplify things.  This
-- won't make much difference in the grand scheme of things since
-- types are small.

-- | Check whether a type contains a specific bound recursive type
--   variable.
containsVar ::
  (Has (Reader TDCtx) sig m, Has (Throw KindError) sig m) =>
  Nat ->
  Type ->
  m Bool
containsVar i ty@(Fix tyF) = case tyF of
  TyRecVarF j -> pure (i == j)
  TyVarF {} -> pure False
  TyConF (TCUser u) tys -> do
    ty' <-
      withThrow
        (\(UnexpandedUserType _) -> UndefinedTyCon (TCUser u) ty)
        (expandTydef u tys)
    containsVar i ty'
  TyConF _ tys -> or <$> mapM (containsVar i) tys
  TyRcdF m -> or <$> mapM (containsVar i) m
  TyRecF _ ty' -> containsVar (NS i) ty'

-- | @nonVacuous ty@ checks that the recursive type @rec x. ty@ is
--   non-vacuous, /i.e./ that it doesn't look like @rec x. x@.  Put
--   another way, we make sure the recursive type is "productive" in
--   the sense that unfolding it will result in a well-defined
--   infinite type (as opposed to @rec x. x@ which just unfolds to
--   itself).  However, we can't just check whether it literally looks
--   like @rec x. x@ since we must also (1) expand type aliases and
--   (2) ignore additional intervening @rec@s.  For example, given
--   @tydef Id a = a@, the type @rec x. rec y. Id x@ is also vacuous.
nonVacuous ::
  (Has (Reader TDCtx) sig m, Has (Throw KindError) sig m) =>
  Nat ->
  Type ->
  m Bool
nonVacuous i ty@(Fix tyF) = case tyF of
  -- The type simply consists of a variable bound by some @rec@.
  -- Check if it's the variable we're currently looking for.
  TyRecVarF j -> pure (i /= j)
  -- Expand a user-defined type and keep looking.
  TyConF (TCUser u) tys -> do
    ty' <-
      withThrow
        (\(UnexpandedUserType _) -> UndefinedTyCon (TCUser u) ty)
        (expandTydef u tys)
    nonVacuous i ty'
  -- Increment the variable we're looking for when going under a @rec@
  -- binder.
  TyRecF _ ty' -> nonVacuous (NS i) ty'
  -- If we encounter any other kind of type constructor or record
  -- type, rejoice!
  TyConF {} -> pure True
  TyRcdF {} -> pure True
  -- This last case can't actully happen if we already checked that
  -- the recursive type actually contains its bound variable (with
  -- 'containsVar'), since it would correspond to something like @rec
  -- x. y@.  However, it's still correct to return True.
  TyVarF {} -> pure True
