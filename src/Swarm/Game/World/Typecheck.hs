{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Typechecking and elaboration for the Swarm world DSL.
module Swarm.Game.World.Typecheck where

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Throw (Throw, throwError)
import Data.Foldable qualified as F
import Data.Functor.Const qualified as F
import Data.Kind (Type)
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Semigroup (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.Equality (TestEquality (..), type (:~:) (Refl))
import Prettyprinter (Doc, pretty, (<+>))
import Swarm.Game.Entity (EntityMap, lookupEntityName)
import Swarm.Game.Terrain (readTerrain)
import Swarm.Game.World.Syntax
import Swarm.Language.Pretty
import Swarm.Util (showT, squote)
import Swarm.Util.Erasable
import Prelude hiding (lookup)

------------------------------------------------------------
-- Type classes for monoidal world values

-- We could use Semigroup and Monoid, but we want to use the two
-- classes separately and make instances for base types, so it's
-- cleaner to just make our own classes (the instances would be
-- orphans if we used Semigroup and Monoid).

class Empty e where
  empty :: e

instance Empty CellVal where
  empty = CellVal mempty mempty mempty

class Over m where
  (<!>) :: m -> m -> m

instance Over Bool where
  _ <!> x = x

instance Over Integer where
  _ <!> x = x

instance Over Double where
  _ <!> x = x

instance Over CellVal where
  CellVal t1 e1 r1 <!> CellVal t2 e2 r2 = CellVal (t1 <> t2) (e1 <> e2) (r1 <> r2)

------------------------------------------------------------
-- Type class for type-indexed application

infixl 1 $$
class Applicable t where
  ($$) :: t (a -> b) -> t a -> t b

------------------------------------------------------------
-- Distinguishing functions and non-functions at the type level

-- In several places, for efficiency we will require something to be
-- not a function, which we can enforce using the 'NotFun' constraint.

type family IsFun a where
  IsFun (_ -> _) = 'True
  IsFun _ = 'False

type NotFun a = IsFun a ~ 'False

------------------------------------------------------------
-- Type-indexed constants

-- | Type-indexed constants.  These include both language built-ins
--   (@if@, arithmetic, comparison, @<>@, etc.) as well as combinators
--   (@S@, @I@, @C@, @K@, @B@, @Φ@) we will use both for elaboration
--   and later as a compilation target.
data Const :: Type -> Type where
  CLit :: (Show a, NotFun a) => a -> Const a
  CCell :: CellVal -> Const CellVal
  -- We have a separate CCell instead of using CLit for cells so that we can
  -- later extract all the entities from a world expression.
  CFI :: Const (Integer -> Double)
  CIf :: Const (Bool -> a -> a -> a)
  CNot :: Const (Bool -> Bool)
  CNeg :: (Num a, NotFun a) => Const (a -> a)
  CAbs :: (Num a, NotFun a) => Const (a -> a)
  CAnd :: Const (Bool -> Bool -> Bool)
  COr :: Const (Bool -> Bool -> Bool)
  CAdd :: (Num a, NotFun a) => Const (a -> a -> a)
  CSub :: (Num a, NotFun a) => Const (a -> a -> a)
  CMul :: (Num a, NotFun a) => Const (a -> a -> a)
  CDiv :: (Fractional a, NotFun a) => Const (a -> a -> a)
  CIDiv :: (Integral a, NotFun a) => Const (a -> a -> a)
  CMod :: (Integral a, NotFun a) => Const (a -> a -> a)
  CEq :: (Eq a, NotFun a) => Const (a -> a -> Bool)
  CNeq :: (Eq a, NotFun a) => Const (a -> a -> Bool)
  CLt :: (Ord a, NotFun a) => Const (a -> a -> Bool)
  CLeq :: (Ord a, NotFun a) => Const (a -> a -> Bool)
  CGt :: (Ord a, NotFun a) => Const (a -> a -> Bool)
  CGeq :: (Ord a, NotFun a) => Const (a -> a -> Bool)
  CMask :: (Empty a, NotFun a) => Const (World Bool -> World a -> World a)
  CSeed :: Const Integer
  CCoord :: Axis -> Const (World Integer)
  CHash :: Const (World Integer)
  CPerlin :: Const (Integer -> Integer -> Double -> Double -> World Double)
  CReflect :: Axis -> Const (World a -> World a)
  CRot :: Rot -> Const (World a -> World a)
  COver :: (Over a, NotFun a) => Const (a -> a -> a)
  -- Combinators generated during elaboration + variable abstraction
  K :: Const (a -> b -> a)
  S :: Const ((a -> b -> c) -> (a -> b) -> a -> c)
  I :: Const (a -> a)
  B :: Const ((b -> c) -> (a -> b) -> a -> c)
  C :: Const ((a -> b -> c) -> b -> a -> c)
  -- Phoenix combinator, aka liftA2.  Including this combinator in the
  -- target set is not typical, but it turns out to be very helpful in
  -- elaborating the "over" operation.
  Φ :: Const ((a -> b -> c) -> (d -> a) -> (d -> b) -> (d -> c))

deriving instance Show (Const ty)

class HasConst t where
  embed :: Const a -> t a

infixl 1 .$$
(.$$) :: (HasConst t, Applicable t) => Const (a -> b) -> t a -> t b
c .$$ t = embed c $$ t

infixl 1 $$.
($$.) :: (HasConst t, Applicable t) => t (a -> b) -> Const a -> t b
t $$. c = t $$ embed c

infixl 1 .$$.
(.$$.) :: (HasConst t, Applicable t) => Const (a -> b) -> Const a -> t b
c1 .$$. c2 = embed c1 $$ embed c2

instance PrettyPrec (Const α) where
  prettyPrec _ = \case
    CLit a -> pretty (showT a)
    CCell c -> ppr c
    CFI -> "fromIntegral"
    CIf -> "if"
    CNot -> "not"
    CNeg -> "negate"
    CAbs -> "abs"
    CAnd -> "and"
    COr -> "or"
    CAdd -> "add"
    CSub -> "sub"
    CMul -> "mul"
    CDiv -> "div"
    CIDiv -> "idiv"
    CMod -> "mod"
    CEq -> "eq"
    CNeq -> "neq"
    CLt -> "lt"
    CLeq -> "leq"
    CGt -> "gt"
    CGeq -> "geq"
    CMask -> "mask"
    CSeed -> "seed"
    CCoord ax -> ppr ax
    CHash -> "hash"
    CPerlin -> "perlin"
    CReflect ax -> case ax of X -> "vreflect"; Y -> "hreflect"
    CRot rot -> ppr rot
    COver -> "over"
    K -> "K"
    S -> "S"
    I -> "I"
    B -> "B"
    C -> "C"
    Φ -> "Φ"

------------------------------------------------------------
-- Intrinsically typed core language

-- | Type-level list append.
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Type- and context-indexed de Bruijn indices. (v :: Idx g a) means
--   v represents a variable with type a in a type context g.
data Idx :: [Type] -> Type -> Type where
  VZ :: Idx (ty ': g) ty
  VS :: Idx g ty -> Idx (x ': g) ty

deriving instance Show (Idx g ty)

idxToNat :: Idx g a -> Int
idxToNat VZ = 0
idxToNat (VS x) = 1 + idxToNat x

-- | A variable valid in one context is also valid in another extended
--   context with additional variables.
weakenVar :: forall h g a. Idx g a -> Idx (Append g h) a
weakenVar VZ = VZ
weakenVar (VS x) = VS (weakenVar @h x)

-- | Type-indexed terms.  Note this is a stripped-down core language,
--   with only variables, lambdas, application, and constants.
data TTerm :: [Type] -> Type -> Type where
  TVar :: Idx g a -> TTerm g a
  TLam :: TTerm (ty1 ': g) ty2 -> TTerm g (ty1 -> ty2)
  TApp :: TTerm g (a -> b) -> TTerm g a -> TTerm g b
  TConst :: Const a -> TTerm g a

deriving instance Show (TTerm g ty)

instance Applicable (TTerm g) where
  TConst I $$ x = x
  f $$ x = TApp f x

instance HasConst (TTerm g) where
  embed = TConst

instance PrettyPrec (TTerm g α) where
  prettyPrec :: Int -> TTerm g α -> Doc ann
  prettyPrec p = \case
    TVar ix -> pretty (idxToNat ix)
    TLam t ->
      pparens (p > 0) $
        "λ." <+> ppr t
    TApp t1 t2 ->
      pparens (p > 1) $
        prettyPrec 1 t1 <+> prettyPrec 2 t2
    TConst c -> ppr c

-- | A term valid in one context is also valid in another extended
--   context with additional variables (which the term does not use).
weaken :: forall h g a. TTerm g a -> TTerm (Append g h) a
weaken (TVar x) = TVar (weakenVar @h x)
weaken (TLam t) = TLam (weaken @h t)
weaken (TApp t1 t2) = TApp (weaken @h t1) (weaken @h t2)
weaken (TConst c) = TConst c

------------------------------------------------------------
-- Errors

-- | Errors that can occur during typechecking/elaboration.
data CheckErr where
  UnknownErr :: Int -> CheckErr
  ApplyErr :: Some (TTerm g) -> Some (TTerm g) -> CheckErr

deriving instance Show CheckErr

-- XXX make into PrettyPrec instance?
prettyCheckErr :: CheckErr -> Text
prettyCheckErr = \case
  ApplyErr (Some ty1 t1) (Some ty2 t2) ->
    T.unlines
      [ "Error in application:"
      , "  " <> squote (prettyText t1) <> " has type " <> squote (prettyText ty1)
      , "  and cannot be applied to"
      , "  " <> squote (prettyText t2) <> " which has type " <> squote (prettyText ty2)
      ]
  UnknownErr n -> "Unknown error #" <> showT n

------------------------------------------------------------
-- Type representations

-- | Base types.
data Base :: Type -> Type where
  BInt :: Base Integer
  BFloat :: Base Double
  BBool :: Base Bool
  BCell :: Base CellVal

deriving instance Show (Base ty)

-- | Testing base type representations for equality to yield reflected
--   type-level equalities.
instance TestEquality Base where
  testEquality BInt BInt = Just Refl
  testEquality BFloat BFloat = Just Refl
  testEquality BBool BBool = Just Refl
  testEquality BCell BCell = Just Refl
  testEquality _ _ = Nothing

instance PrettyPrec (Base α) where
  prettyPrec _ = \case
    BInt -> "int"
    BFloat -> "float"
    BBool -> "bool"
    BCell -> "cell"

-- | Type representations indexed by the corresponding host language
--   type.
data TTy :: Type -> Type where
  TTyBase :: Base t -> TTy t
  (:->:) :: TTy a -> TTy b -> TTy (a -> b)
  TTyWorld :: TTy t -> TTy (World t)

infixr 0 :->:

pattern TTyBool :: TTy Bool
pattern TTyBool = TTyBase BBool

pattern TTyInt :: TTy Integer
pattern TTyInt = TTyBase BInt

pattern TTyFloat :: TTy Double
pattern TTyFloat = TTyBase BFloat

pattern TTyCell :: TTy CellVal
pattern TTyCell = TTyBase BCell

deriving instance Show (TTy ty)

-- | Testing type representations for equality to yield reflected
--   type-level equalities.
instance TestEquality TTy where
  testEquality (TTyBase b1) (TTyBase b2) = testEquality b1 b2
  testEquality (TTyWorld b1) (TTyWorld b2) =
    case testEquality b1 b2 of
      Just Refl -> Just Refl
      Nothing -> Nothing
  testEquality _ _ = Nothing

instance PrettyPrec (TTy ty) where
  prettyPrec :: Int -> TTy ty -> Doc ann
  prettyPrec _ (TTyBase b) = ppr b
  prettyPrec p (α :->: β) =
    pparens (p > 0) $
      prettyPrec 1 α <+> "->" <+> prettyPrec 0 β
  prettyPrec p (TTyWorld t) =
    pparens (p > 1) $
      "World" <+> prettyPrec 2 t

------------------------------------------------------------
-- Instance checking

-- | Check that a particular type has an 'Eq' instance, and run a
--   computation in a context provided with an 'Eq' constraint. The
--   other @checkX@ functions are similar.
checkEq :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Eq ty, NotFun ty) => m a) -> m a
checkEq (TTyBase BBool) a = a
checkEq (TTyBase BInt) a = a
checkEq (TTyBase BFloat) a = a
checkEq _ _ = throwError (UnknownErr 0)

checkOrd :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Ord ty, NotFun ty) => m a) -> m a
checkOrd (TTyBase BBool) a = a
checkOrd (TTyBase BInt) a = a
checkOrd (TTyBase BFloat) a = a
checkOrd _ _ = throwError (UnknownErr 1)

checkNum :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Num ty, NotFun ty) => m a) -> m a
checkNum (TTyBase BInt) a = a
checkNum (TTyBase BFloat) a = a
checkNum _ _ = throwError (UnknownErr 2)

checkIntegral :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Integral ty, NotFun ty) => m a) -> m a
checkIntegral (TTyBase BInt) a = a
checkIntegral _ _ = throwError (UnknownErr 3)

checkEmpty :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Empty ty, NotFun ty) => m a) -> m a
checkEmpty (TTyBase BCell) a = a
checkEmpty _ _ = throwError (UnknownErr 4)

checkOver :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Over ty, NotFun ty) => m a) -> m a
checkOver (TTyBase BBool) a = a
checkOver (TTyBase BInt) a = a
checkOver (TTyBase BFloat) a = a
checkOver (TTyBase BCell) a = a
checkOver _ _ = throwError (UnknownErr 5)

------------------------------------------------------------
-- Existential wrappers

-- | Wrap up a type-indexed thing to hide the type index, but package
--   it with a 'TTy' which we can pattern-match on to recover the type
--   later.
data Some :: (Type -> Type) -> Type where
  Some :: TTy α -> t α -> Some t

deriving instance (forall α. Show (t α)) => Show (Some t)

mapSome :: (forall α. s α -> t α) -> Some s -> Some t
mapSome f (Some ty t) = Some ty (f t)

type SomeTy = Some (F.Const ())

pattern SomeTy :: TTy α -> SomeTy
pattern SomeTy α = Some α (F.Const ())
{-# COMPLETE SomeTy #-}

------------------------------------------------------------
-- Type inference/checking + elaboration

type WExpMap = Map Text (Some (TTerm '[]))

-- | Type contexts, indexed by a type-level list of types of all the
--   variables in the context.
data Ctx :: [Type] -> Type where
  CNil :: Ctx '[]
  CCons :: Text -> TTy ty -> Ctx g -> Ctx (ty ': g)

-- | Look up a variable name in the context, returning a type-indexed
--   de Bruijn index.
lookup :: (Has (Throw CheckErr) sig m) => Text -> Ctx g -> m (Some (Idx g))
lookup _ CNil = throwError (UnknownErr 6)
lookup x (CCons y ty ctx)
  | x == y = return $ Some ty VZ
  | otherwise = mapSome VS <$> lookup x ctx

-- | Check that a term has a given type, and if so, return a
--   corresponding elaborated and type-indexed term.  Note that this
--   also deals with subtyping: for example, if we check that the term
--   @3@ has type @World Int@, we will get back a suitably lifted
--   value (/i.e./ @const 3@).
check ::
  ( Has (Throw CheckErr) sig m
  , Has (Reader EntityMap) sig m
  , Has (Reader WExpMap) sig m
  ) =>
  Ctx g ->
  TTy t ->
  WExp ->
  m (TTerm g t)
check e ty t = do
  t1 <- infer e t
  Some ty' t' <- apply (Some (ty :->: ty) (embed I)) t1
  case testEquality ty ty' of
    Nothing -> throwError (UnknownErr 7)
    Just Refl -> return t'

-- | Get the underlying base type of a term which either has a base
--   type or a World type.
getBaseType :: Some (TTerm g) -> SomeTy
getBaseType (Some (TTyWorld ty) _) = SomeTy ty
getBaseType (Some ty _) = SomeTy ty

-- | Apply one term to another term, automatically handling promotion
--   and lifting, via the fact that World is Applicative.  That is,
--   (1) if a term of type T is used where a term of type World T is
--   expected, it will automatically be promoted (by an application of
--   const); (2) if a function of type (T1 -> T2 -> ... -> Tn) is
--   applied to any arguments of type (World Ti), the function will be
--   lifted to (World T1 -> World T2 -> ... -> World Tn).
apply :: (Has (Throw CheckErr) sig m) => Some (TTerm g) -> Some (TTerm g) -> m (Some (TTerm g))
-- Normal function application
apply (Some (ty11 :->: ty12) t1) (Some ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some ty12 (t1 $$ t2)
-- (World T -> ...) applied to T: promote the argument to (World T) with const
apply (Some (TTyWorld ty11 :->: ty12) t1) (Some ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some ty12 (t1 $$ (K .$$ t2))
-- (S -> T) applied to (World S): lift the function to (World S -> World T).
apply (Some (ty11 :->: ty12) t1) (Some (TTyWorld ty2) t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some (TTyWorld ty12) (B .$$ t1 $$ t2)
-- World (S -> T) applied to S.  Note this case and the next are
-- needed because in the previous case, when (S -> T) is lifted to
-- (World S -> World T), T may itself be a function type.
apply (Some (TTyWorld (ty11 :->: ty12)) t1) (Some ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some (TTyWorld ty12) (S .$$ t1 $$ (K .$$ t2))
-- World (S -> T) applied to (World S)
apply (Some (TTyWorld (ty11 :->: ty12)) t1) (Some (TTyWorld ty2) t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some (TTyWorld ty12) (S .$$ t1 $$ t2)
apply t1 t2 = throwError $ ApplyErr t1 t2

applyTo :: (Has (Throw CheckErr) sig m) => Some (TTerm g) -> Some (TTerm g) -> m (Some (TTerm g))
applyTo = flip apply

-- | Infer the type of an operator: turn a raw operator into a
--   type-indexed constant.  However, some operators are polymorphic,
--   so we also provide a list of type arguments.  For example, the
--   type of the negation operator can be either (Int -> Int) or
--   (Float -> Float) so we provide it as an argument.
--
--   Currently, all operators take at most one type argument, so
--   (Maybe SomeTy) might seem more appropriate than [SomeTy], but
--   that is just a coincidence; in general one can easily imagine
--   operators that are polymorphic in more than one type variable,
--   and we may wish to add such in the future.
inferOp :: (Has (Throw CheckErr) sig m) => [SomeTy] -> Op -> m (Some (TTerm g))
inferOp _ Not = return $ Some (TTyBool :->: TTyBool) (embed CNot)
inferOp [SomeTy tyA] Neg = Some (tyA :->: tyA) <$> checkNum tyA (return $ embed CNeg)
inferOp _ And = return $ Some (TTyBool :->: TTyBool :->: TTyBool) (embed CAnd)
inferOp _ Or = return $ Some (TTyBool :->: TTyBool :->: TTyBool) (embed COr)
inferOp [SomeTy tyA] Abs = Some (tyA :->: tyA) <$> checkNum tyA (return $ embed CAbs)
inferOp [SomeTy tyA] Add = Some (tyA :->: tyA :->: tyA) <$> checkNum tyA (return $ embed CAdd)
inferOp [SomeTy tyA] Sub = Some (tyA :->: tyA :->: tyA) <$> checkNum tyA (return $ embed CSub)
inferOp [SomeTy tyA] Mul = Some (tyA :->: tyA :->: tyA) <$> checkNum tyA (return $ embed CMul)
inferOp [SomeTy tyA] Div = case tyA of
  TTyBase BInt -> return $ Some (tyA :->: tyA :->: tyA) (embed CIDiv)
  TTyBase BFloat -> return $ Some (tyA :->: tyA :->: tyA) (embed CDiv)
  _ -> throwError (UnknownErr 9)
inferOp [SomeTy tyA] Mod = Some (tyA :->: tyA :->: tyA) <$> checkIntegral tyA (return $ embed CMod)
inferOp [SomeTy tyA] Eq = Some (tyA :->: tyA :->: TTyBool) <$> checkEq tyA (return $ embed CEq)
inferOp [SomeTy tyA] Neq = Some (tyA :->: tyA :->: TTyBool) <$> checkEq tyA (return $ embed CNeq)
inferOp [SomeTy tyA] Lt = Some (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (return $ embed CLt)
inferOp [SomeTy tyA] Leq = Some (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (return $ embed CLeq)
inferOp [SomeTy tyA] Gt = Some (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (return $ embed CGt)
inferOp [SomeTy tyA] Geq = Some (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (return $ embed CGeq)
inferOp [SomeTy tyA] If = return $ Some (TTyBool :->: tyA :->: tyA :->: tyA) (embed CIf)
inferOp _ Perlin = return $ Some (TTyInt :->: TTyInt :->: TTyFloat :->: TTyFloat :->: TTyWorld TTyFloat) (embed CPerlin)
inferOp [SomeTy tyA] (Reflect r) = return $ Some (TTyWorld tyA :->: TTyWorld tyA) (embed (CReflect r))
inferOp [SomeTy tyA] (Rot r) = return $ Some (TTyWorld tyA :->: TTyWorld tyA) (embed (CRot r))
inferOp [SomeTy tyA] Mask = Some (TTyWorld TTyBool :->: TTyWorld tyA :->: TTyWorld tyA) <$> checkEmpty tyA (return $ embed CMask)
inferOp [SomeTy tyA] Overlay = Some (tyA :->: tyA :->: tyA) <$> checkOver tyA (return $ embed COver)
inferOp tys op = error $ "bad call to inferOp: " ++ show tys ++ " " ++ show op

-- | Given a raw operator and the terms the operator is applied to,
--   select which types should be supplied as the type arguments to
--   the operator.  For example, for an operator like @+@ we can just
--   select the type of its first argument; for an operator like @if@,
--   we must select the type of its second argument, since @if : Bool
--   -> a -> a -> a@.  In all cases we must also select the underlying
--   base type in case the argument has a @World@ type.  For example
--   if @+@ is applied to an argument of type @World Int@ we still
--   want to give @+@ the type @Int -> Int -> Int@.  It can be lifted
--   to have type @World Int -> World Int -> World Int@ but that will
--   be taken care of by application, which will insert the right
--   combinators to do the lifting.
typeArgsFor :: Op -> [Some (TTerm g)] -> [SomeTy]
typeArgsFor op (t : _)
  | op `elem` [Neg, Abs, Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Leq, Gt, Geq] = [getBaseType t]
typeArgsFor (Reflect _) (t : _) = [getBaseType t]
typeArgsFor (Rot _) (t : _) = [getBaseType t]
typeArgsFor op (_ : t : _)
  | op `elem` [If, Mask, Overlay] = [getBaseType t]
typeArgsFor _ _ = []

-- | Typecheck the application of an operator to some terms, returning
--   a typed, elaborated version of the application.
applyOp ::
  ( Has (Throw CheckErr) sig m
  , Has (Reader EntityMap) sig m
  , Has (Reader WExpMap) sig m
  ) =>
  Ctx g ->
  Op ->
  [WExp] ->
  m (Some (TTerm g))
applyOp ctx op ts = do
  tts <- mapM (infer ctx) ts
  foldl (\r -> (r >>=) . applyTo) (inferOp (typeArgsFor op tts) op) tts

-- | Infer the type of a term, and elaborate along the way.
infer ::
  forall sig m g.
  ( Has (Throw CheckErr) sig m
  , Has (Reader EntityMap) sig m
  , Has (Reader WExpMap) sig m
  ) =>
  Ctx g ->
  WExp ->
  m (Some (TTerm g))
infer _ (WInt i) = return $ Some (TTyBase BInt) (embed (CLit i))
infer _ (WFloat f) = return $ Some (TTyBase BFloat) (embed (CLit f))
infer _ (WBool b) = return $ Some (TTyBase BBool) (embed (CLit b))
infer _ (WCell c) = do
  c' <- resolveCell c
  return $ Some TTyCell (embed (CCell c'))
infer ctx (WVar x) = mapSome TVar <$> lookup x ctx
infer ctx (WOp op ts) = applyOp ctx op ts
infer _ WSeed = return $ Some TTyInt (embed CSeed)
infer _ (WCoord ax) = return $ Some (TTyWorld TTyInt) (embed (CCoord ax))
infer _ WHash = return $ Some (TTyWorld TTyInt) (embed CHash)
infer ctx (WLet defs body) = inferLet ctx defs body
infer ctx (WOverlay ts) = inferOverlay ctx ts
infer _ctx (WImport key) = do
  wexpMap <- ask @WExpMap
  case M.lookup key wexpMap of
    Just (Some ty t) -> return (Some ty (weaken @g t))
    Nothing -> throwError (UnknownErr 14)

-- | Try to resolve a 'RawCellVal'---containing only 'Text' names for
--   terrain, entities, and robots---into a real 'CellVal' with
--   references to actual terrain, entities, and robots.
resolveCell ::
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
  RawCellVal ->
  m CellVal
resolveCell items = do
  cellVals <- mapM resolveCellItem items
  return $ foldl' (<!>) empty cellVals

-- | Try to resolve one cell item name into an actual item (terrain,
-- entity, robot, etc.).
resolveCellItem ::
  forall sig m.
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
  (Maybe CellTag, Text) ->
  m CellVal
resolveCellItem (mCellTag, item) = case mCellTag of
  Just cellTag -> do
    -- The item was tagged specifically, like {terrain: dirt} or {entity: water}
    mCell <- resolverByTag cellTag item
    maybe (throwError (UnknownErr 12)) return mCell -- cell item is not a 'cellTag'
  Nothing -> do
    -- The item was not tagged; try resolving in all possible ways and choose
    -- the first that works
    maybeCells <- mapM (`resolverByTag` item) [minBound .. maxBound :: CellTag]
    case F.asum maybeCells of
      Nothing -> throwError (UnknownErr 13) -- cell item does not refer to anything
      Just cell -> return cell
 where
  mkTerrain t = CellVal t mempty mempty
  mkEntity e = CellVal mempty (EJust (Last e)) mempty
  resolverByTag :: CellTag -> Text -> m (Maybe CellVal)
  resolverByTag = \case
    CellTerrain -> return . fmap mkTerrain . readTerrain
    CellEntity -> \eName ->
      case eName of
        "erase" -> return $ Just (CellVal mempty EErase mempty)
        _ -> do
          em <- ask @EntityMap
          return . fmap mkEntity $ lookupEntityName eName em
    CellRobot -> \_ -> return Nothing -- TODO (#1396): support robots

-- | Infer the type of a let expression, and elaborate into a series
--   of lambda applications.
inferLet ::
  ( Has (Throw CheckErr) sig m
  , Has (Reader EntityMap) sig m
  , Has (Reader WExpMap) sig m
  ) =>
  Ctx g ->
  [(Var, WExp)] ->
  WExp ->
  m (Some (TTerm g))
inferLet ctx [] body = infer ctx body
inferLet ctx ((x, e) : xs) body = do
  e'@(Some ty1 _) <- infer ctx e
  Some ty2 let' <- inferLet (CCons x ty1 ctx) xs body
  apply (Some (ty1 :->: ty2) (TLam let')) e'

-- | Infer the type of an @overlay@ expression, and elaborate into a
--   chain of @<>@ (over) operations.
inferOverlay ::
  ( Has (Throw CheckErr) sig m
  , Has (Reader EntityMap) sig m
  , Has (Reader WExpMap) sig m
  ) =>
  Ctx g ->
  NE.NonEmpty WExp ->
  m (Some (TTerm g))
inferOverlay ctx es = case NE.uncons es of
  -- @overlay [e] = e@
  (e, Nothing) -> infer ctx e
  -- @overlay (e : es') = e <> overlay es'@
  (e, Just es') -> do
    e' <- infer ctx e
    o' <- inferOverlay ctx es'
    case getBaseType e' of
      SomeTy ty -> do
        let wty = TTyWorld ty
        c <- checkOver ty (return $ embed COver)
        apply (Some (wty :->: wty :->: wty) (Φ .$$ c)) e' >>= applyTo o'
