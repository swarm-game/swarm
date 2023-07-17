{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Type.Equality (TestEquality (..), type (:~:) (Refl))
import Swarm.Game.Entity (EntityMap, lookupEntityName)
import Swarm.Game.Terrain (readTerrain)
import Swarm.Game.World.Syntax
import Prelude hiding (lookup)

------------------------------------------------------------
-- Type class for type-indexed application

infixl 1 $$
class Applicable t where
  ($$) :: t (a -> b) -> t a -> t b

------------------------------------------------------------
-- Type-indexed constants

-- Includes language built-ins as well as combinators we will use
-- later as a compilation target.
data Const :: Type -> Type where
  CLit :: (Show a) => a -> Const a
  CFI :: Const (Integer -> Double)
  CIf :: Const (Bool -> a -> a -> a)
  CNot :: Const (Bool -> Bool)
  CNeg :: (Num a) => Const (a -> a)
  CAbs :: (Num a) => Const (a -> a)
  CAnd :: Const (Bool -> Bool -> Bool)
  COr :: Const (Bool -> Bool -> Bool)
  CAdd :: (Num a) => Const (a -> a -> a)
  CSub :: (Num a) => Const (a -> a -> a)
  CMul :: (Num a) => Const (a -> a -> a)
  CDiv :: (Fractional a) => Const (a -> a -> a)
  CIDiv :: (Integral a) => Const (a -> a -> a)
  CMod :: (Integral a) => Const (a -> a -> a)
  CEq :: (Eq a) => Const (a -> a -> Bool)
  CNeq :: (Eq a) => Const (a -> a -> Bool)
  CLt :: (Ord a) => Const (a -> a -> Bool)
  CLeq :: (Ord a) => Const (a -> a -> Bool)
  CGt :: (Ord a) => Const (a -> a -> Bool)
  CGeq :: (Ord a) => Const (a -> a -> Bool)
  CMask :: (Empty a) => Const (World Bool -> World a -> World a)
  CSeed :: Const Integer
  CCoord :: Axis -> Const (World Integer)
  CHash :: Const (World Integer)
  CPerlin :: Const (Integer -> Integer -> Double -> Double -> World Double)
  CReflect :: Axis -> Const (World a -> World a)
  CRot :: Rot -> Const (World a -> World a)
  COver :: (Over a) => Const (a -> a -> a)
  CEmpty :: (Empty a) => Const a
  K :: Const (a -> b -> a)
  S :: Const ((a -> b -> c) -> (a -> b) -> a -> c)
  I :: Const (a -> a)
  B :: Const ((b -> c) -> (a -> b) -> a -> c)
  C :: Const ((a -> b -> c) -> b -> a -> c)
  Φ :: Const ((a -> b -> c) -> (d -> a) -> (d -> b) -> (d -> c)) -- Phoenix, aka liftA2

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

------------------------------------------------------------
-- Intrinsically typed core language

-- Typed de Bruijn indices.
data Idx :: [Type] -> Type -> Type where
  VZ :: Idx (ty ': g) ty
  VS :: Idx g ty -> Idx (x ': g) ty

deriving instance Show (Idx g ty)

-- Type-indexed terms.  Note this is a stripped-down core language,
-- with only variables, lambdas, application, and constants.
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

------------------------------------------------------------
-- Errors

data CheckErr where
  UnknownErr :: Int -> CheckErr
  ApplyErr :: Some (TTerm g) -> Some (TTerm g) -> CheckErr

deriving instance Show CheckErr

------------------------------------------------------------
-- Type representations

data Base :: Type -> Type where
  BInt :: Base Integer
  BFloat :: Base Double
  BBool :: Base Bool
  BCell :: Base CellVal

deriving instance Show (Base ty)

instance TestEquality Base where
  testEquality BInt BInt = Just Refl
  testEquality BFloat BFloat = Just Refl
  testEquality BBool BBool = Just Refl
  testEquality BCell BCell = Just Refl
  testEquality _ _ = Nothing

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

instance TestEquality TTy where
  testEquality (TTyBase b1) (TTyBase b2) = testEquality b1 b2
  testEquality (TTyWorld b1) (TTyWorld b2) =
    case testEquality b1 b2 of
      Just Refl -> Just Refl
      Nothing -> Nothing
  testEquality _ _ = Nothing

checkEq :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Eq ty) => m a) -> m a
checkEq (TTyBase BBool) a = a
checkEq (TTyBase BInt) a = a
checkEq (TTyBase BFloat) a = a
checkEq _ _ = throwError (UnknownErr 0)

checkOrd :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Ord ty) => m a) -> m a
checkOrd (TTyBase BBool) a = a
checkOrd (TTyBase BInt) a = a
checkOrd (TTyBase BFloat) a = a
checkOrd _ _ = throwError (UnknownErr 1)

checkNum :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Num ty) => m a) -> m a
checkNum (TTyBase BInt) a = a
checkNum (TTyBase BFloat) a = a
checkNum _ _ = throwError (UnknownErr 2)

checkIntegral :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Integral ty) => m a) -> m a
checkIntegral (TTyBase BInt) a = a
checkIntegral _ _ = throwError (UnknownErr 3)

checkEmpty :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Empty ty) => m a) -> m a
checkEmpty (TTyBase BCell) a = a
checkEmpty _ _ = throwError (UnknownErr 4)

checkOver :: (Has (Throw CheckErr) sig m) => TTy ty -> ((Over ty) => m a) -> m a
checkOver (TTyBase BBool) a = a
checkOver (TTyBase BInt) a = a
checkOver (TTyBase BFloat) a = a
checkOver (TTyBase BCell) a = a
checkOver _ _ = throwError (UnknownErr 5)

------------------------------------------------------------
-- Existential wrappers

data Some :: (Type -> Type) -> Type where
  Some :: TTy α -> t α -> Some t

deriving instance (forall α. Show (t α)) => Show (Some t)

mapSome :: (forall α. s α -> t α) -> Some s -> Some t
mapSome f (Some ty t) = Some ty (f t)

type SomeTy = Some (F.Const ())

pattern SomeTy :: TTy α -> SomeTy
pattern SomeTy α = Some α (F.Const ())
{-# COMPLETE SomeTy #-}

data Ctx :: [Type] -> Type where
  CNil :: Ctx '[]
  CCons :: Text -> TTy ty -> Ctx g -> Ctx (ty ': g)

lookup :: (Has (Throw CheckErr) sig m) => Text -> Ctx g -> m (Some (Idx g))
lookup _ CNil = throwError (UnknownErr 6)
lookup x (CCons y ty ctx)
  | x == y = return $ Some ty VZ
  | otherwise = mapSome VS <$> lookup x ctx

------------------------------------------------------------
-- Type inference/checking + elaboration

check ::
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
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

-- -- For my own sanity I think we might get rid of the rule Int <: Float
-- -- and only allow promoting/lifting to World.  Then 3 will always be
-- -- Int and 3.0 will be Float.

getBaseType :: Some (TTerm g) -> SomeTy
getBaseType (Some (TTyWorld ty) _) = SomeTy ty
getBaseType (Some ty _) = SomeTy ty

-- Application is where we deal with lifting + promotion.
apply :: (Has (Throw CheckErr) sig m) => Some (TTerm g) -> Some (TTerm g) -> m (Some (TTerm g))
apply (Some (ty11 :->: ty12) t1) (Some ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some ty12 (t1 $$ t2)
apply (Some (TTyWorld ty11 :->: ty12) t1) (Some ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some ty12 (t1 $$ (K .$$ t2))
apply (Some (ty11 :->: ty12) t1) (Some (TTyWorld ty2) t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some (TTyWorld ty12) (B .$$ t1 $$ t2)
apply (Some (TTyWorld (ty11 :->: ty12)) t1) (Some ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some (TTyWorld ty12) (S .$$ t1 $$ (K .$$ t2))
apply (Some (TTyWorld (ty11 :->: ty12)) t1) (Some (TTyWorld ty2) t2)
  | Just Refl <- testEquality ty11 ty2 = return $ Some (TTyWorld ty12) (S .$$ t1 $$ t2)
apply t1 t2 = throwError $ ApplyErr t1 t2

applyTo :: (Has (Throw CheckErr) sig m) => Some (TTerm g) -> Some (TTerm g) -> m (Some (TTerm g))
applyTo = flip apply

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

typeArgsFor :: Op -> [Some (TTerm g)] -> [SomeTy]
typeArgsFor op (t : _)
  | op `elem` [Neg, Abs, Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Leq, Gt, Geq] = [getBaseType t]
typeArgsFor (Reflect _) (t : _) = [getBaseType t]
typeArgsFor (Rot _) (t : _) = [getBaseType t]
typeArgsFor op (_ : t : _)
  | op `elem` [If, Mask, Overlay] = [getBaseType t]
typeArgsFor _ _ = []

applyOp ::
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
  Ctx g ->
  ([Some (TTerm g)] -> [SomeTy]) ->
  Op ->
  [WExp] ->
  m (Some (TTerm g))
applyOp ctx typeArgs op ts = do
  tts <- mapM (infer ctx) ts
  foldl (\r -> (r >>=) . applyTo) (inferOp (typeArgs tts) op) tts

infer ::
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
  Ctx g ->
  WExp ->
  m (Some (TTerm g))
infer _ (WInt i) = return $ Some (TTyBase BInt) (embed (CLit i))
infer _ (WFloat f) = return $ Some (TTyBase BFloat) (embed (CLit f))
infer _ (WBool b) = return $ Some (TTyBase BBool) (embed (CLit b))
infer _ (WCell c) = do
  c' <- resolveCell c
  return $ Some TTyCell (embed (CLit c')) -- XXX resolve cell
infer ctx (WVar x) = mapSome TVar <$> lookup x ctx
infer ctx (WOp op ts) = applyOp ctx (typeArgsFor op) op ts
infer _ WSeed = return $ Some TTyInt (embed CSeed)
infer _ (WCoord ax) = return $ Some (TTyWorld TTyInt) (embed (CCoord ax))
infer _ WHash = return $ Some (TTyWorld TTyInt) (embed CHash)
infer ctx (WLet defs body) = inferLet ctx defs body
infer ctx (WOverlay ts) = inferOverlay ctx ts
infer _ctx (WCat _ax _ts) = undefined
infer _ctx (WStruct _pal _rect) = undefined

resolveCell ::
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
  RawCellVal ->
  m CellVal
resolveCell items = do
  cellVals <- mapM resolveCellItem items
  return $ foldl' (<+>) empty cellVals

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
  mkTerrain t = CellVal (Last (Just t)) mempty mempty
  mkEntity e = CellVal mempty (Last (Just e)) mempty
  resolverByTag :: CellTag -> Text -> m (Maybe CellVal)
  resolverByTag = \case
    CellTerrain -> return . fmap mkTerrain . readTerrain
    CellEntity -> \eName -> do
      em <- ask @EntityMap
      return . fmap mkEntity $ lookupEntityName eName em
    CellRobot -> \_ -> return Nothing -- XXX robots!

inferLet ::
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
  Ctx g ->
  [(Var, WExp)] ->
  WExp ->
  m (Some (TTerm g))
inferLet ctx [] body = infer ctx body
inferLet ctx ((x, e) : xs) body = do
  e'@(Some ty1 _) <- infer ctx e
  Some ty2 let' <- inferLet (CCons x ty1 ctx) xs body
  apply (Some (ty1 :->: ty2) (TLam let')) e'

inferOverlay ::
  (Has (Throw CheckErr) sig m, Has (Reader EntityMap) sig m) =>
  Ctx g ->
  NE.NonEmpty WExp ->
  m (Some (TTerm g))
inferOverlay ctx es = case NE.uncons es of
  (e, Nothing) -> infer ctx e
  (e, Just es') -> do
    e' <- infer ctx e
    o' <- inferOverlay ctx es'
    case getBaseType e' of
      SomeTy ty -> do
        let wty = TTyWorld ty
        c <- checkOver ty (return $ embed COver)
        apply (Some (wty :->: wty :->: wty) (Φ .$$ c)) e' >>= applyTo o'
