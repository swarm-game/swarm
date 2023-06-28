{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
module Swarm.Game.World.DSL where

import Data.Int
import Data.Kind (Type)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Type.Equality (TestEquality (..), type (:~:) (Refl))
import Swarm.Game.Scenario.WorldPalette
import Swarm.Game.Terrain
import Prelude hiding (lookup)

------------------------------------------------------------
-- Merging

class Empty e where
  empty :: e

class Mergeable m where
  (<+>) :: m -> m -> m

------------------------------------------------------------
-- Syntax

data CellVal e r = CellVal (Last TerrainType) (Last e) [r]

instance Mergeable (CellVal e r) where
  CellVal t1 e1 r1 <+> CellVal t2 e2 r2 = CellVal (t1 <> t2) (e1 <> e2) (r1 <> r2)

instance Empty (CellVal e r) where
  empty = CellVal mempty mempty mempty

instance Mergeable Bool where
  _ <+> x = x

instance Mergeable Integer where
  _ <+> x = x

instance Mergeable Double where
  _ <+> x = x

type RawCellVal = CellVal Text Text

data Rot = Rot0 | Rot90 | Rot180 | Rot270
  deriving (Eq, Ord, Show, Bounded, Enum)

data Reflection = ReflectH | ReflectV
  deriving (Eq, Ord, Show, Bounded, Enum)

type Var = Text

data Axis = X | Y
  deriving (Eq, Ord, Show, Bounded, Enum)

data WExp where
  WInt :: Integer -> WExp
  WFloat :: Double -> WExp
  WBool :: Bool -> WExp
  WCell :: RawCellVal -> WExp
  WVar :: Text -> WExp
  WOp :: Op -> [WExp] -> WExp
  WSeed :: WExp
  WCoord :: Axis -> WExp
  WHash :: WExp
  WLet :: [(Var, WExp)] -> WExp -> WExp
  WOverlay :: [WExp] -> WExp
  WCat :: Axis -> [WExp] -> WExp
  WStruct :: WorldPalette Text -> [Text] -> WExp

data Op = Not | Neg | And | Or | Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | If | Perlin | Reflect Reflection | Rot Rot | Mask
  deriving (Eq, Ord, Show)

------------------------------------------------------------
-- Example

testWorld1 :: WExp
testWorld1 =
  WLet
    [ ("pn1", WOp Perlin [WInt 0, WInt 5, WFloat 0.05, WFloat 0.5])
    , ("pn2", WOp Perlin [WInt 0, WInt 5, WFloat 0.05, WFloat 0.75])
    ]
    $ WOverlay
      [ WCell (CellVal (Last (Just GrassT)) (Last Nothing) [])
      , WOp Mask [WOp Gt [WVar "pn2", WFloat 0], WCell (CellVal (Last (Just StoneT)) (Last (Just "rock")) [])]
      , WOp Mask [WOp Gt [WVar "pn1", WFloat 0], WCell (CellVal (Last (Just DirtT)) (Last (Just "tree")) [])]
      , WOp
          Mask
          [ WOp And [WOp Eq [WCoord X, WInt 2], WOp Eq [WCoord Y, WInt (-1)]]
          , WCell (CellVal (Last (Just GrassT)) (Last (Just "elephant")) [])
          ]
      , WOp
          Mask
          [ WOp And [WOp Eq [WCoord X, WInt (-5)], WOp Eq [WCoord Y, WInt 3]]
          , WCell (CellVal (Last (Just StoneT)) (Last (Just "flerb")) [])
          ]
      ]

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
  CMask :: Const (World Bool -> World a -> World a) -- XXX make our own Empty + Combining classes, add constraint
  CSeed :: Const Integer
  CCoord :: Axis -> Const (World Integer)
  CHash :: Const (World Integer)
  CPerlin :: Const (Integer -> Integer -> Double -> Double -> World Double)
  CReflect :: Reflection -> Const (World a -> World a)
  CRot :: Rot -> Const (World a -> World a)
  K :: Const (a -> b -> a)
  S :: Const ((a -> b -> c) -> (a -> b) -> a -> c)
  I :: Const (a -> a)
  B :: Const ((b -> c) -> (a -> b) -> a -> c)
  C :: Const ((a -> b -> c) -> b -> a -> c)

deriving instance Show (Const ty)

-- Interpret constants directly into the host language.  We don't use
-- this in our ultimate compilation but it's nice to have for
-- debugging/comparison.
interpConst :: Const ty -> ty
interpConst = \case
  CLit a -> a
  CIf -> \b t e -> if b then t else e
  CNot -> not
  CNeg -> negate
  CAnd -> (&&)
  COr -> (||)
  CAdd -> (+)
  CSub -> (-)
  CMul -> (*)
  CDiv -> (/)
  CIDiv -> div
  CMod -> mod
  CEq -> (==)
  CNeq -> (/=)
  CLt -> (<)
  CLeq -> (<=)
  CGt -> (>)
  CGeq -> (>=)
  CMask -> undefined -- (\b x c -> if b c then x c else empty)
  CSeed -> 0 -- XXX need seed provided as env to be able to interpret this
  CCoord ax -> undefined -- \(Coords (x,y)) -> case ax of X -> x; Y -> y  -- XXX Integer vs Int32
  CHash -> undefined
  CPerlin -> undefined
  CReflect _ -> undefined
  CRot _ -> undefined
  CFI -> fromInteger
  K -> const
  S -> (<*>)
  I -> id
  B -> (.)
  C -> flip

class HasConst t where
  embed :: Const a -> t a

infixl 1 .$
(.$) :: (HasConst t, Applicable t) => Const (a -> b) -> t a -> t b
c .$ t = embed c $$ t

infixl 1 $.
($.) :: (HasConst t, Applicable t) => t (a -> b) -> Const a -> t b
t $. c = t $$ embed c

infixl 1 .$.
(.$.) :: (HasConst t, Applicable t) => Const (a -> b) -> Const a -> t b
c1 .$. c2 = embed c1 $$ embed c2

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
-- Type representations

newtype Coords = Coords {unCoords :: (Int32, Int32)} -- XXX
type World b = Coords -> b

data Base :: Type -> Type where
  BInt :: Base Integer
  BFloat :: Base Double
  BBool :: Base Bool

deriving instance Show (Base ty)

instance TestEquality Base where
  testEquality BInt BInt = Just Refl
  testEquality BFloat BFloat = Just Refl
  testEquality BBool BBool = Just Refl
  testEquality _ _ = Nothing

data TType :: Type -> Type where
  TTyBase :: Base t -> TType t
  (:->:) :: TType a -> TType b -> TType (a -> b)
  TTyWorld :: TType t -> TType (World t)

infixr 0 :->:

pattern TTyBool :: TType Bool
pattern TTyBool = TTyBase BBool

pattern TTyInt :: TType Integer
pattern TTyInt = TTyBase BInt

pattern TTyFloat :: TType Double
pattern TTyFloat = TTyBase BFloat

deriving instance Show (TType ty)

instance TestEquality TType where
  testEquality (TTyBase b1) (TTyBase b2) = testEquality b1 b2
  testEquality (TTyWorld b1) (TTyWorld b2) =
    case testEquality b1 b2 of
      Just Refl -> Just Refl
      Nothing -> Nothing
  testEquality _ _ = Nothing

checkEq :: TType ty -> ((Eq ty) => a) -> Maybe a
checkEq (TTyBase BBool) a = Just a
checkEq (TTyBase BInt) a = Just a
checkEq (TTyBase BFloat) a = Just a
checkEq _ _ = Nothing

checkOrd :: TType ty -> ((Ord ty) => a) -> Maybe a
checkOrd (TTyBase BBool) a = Just a
checkOrd (TTyBase BInt) a = Just a
checkOrd (TTyBase BFloat) a = Just a
checkOrd _ _ = Nothing

checkNum :: TType ty -> ((Num ty) => a) -> Maybe a
checkNum (TTyBase BInt) a = Just a
checkNum (TTyBase BFloat) a = Just a
checkNum _ _ = Nothing

checkIntegral :: TType ty -> ((Integral ty) => a) -> Maybe a
checkIntegral (TTyBase BInt) a = Just a
checkIntegral _ _ = Nothing

data SomeType :: Type where
  SomeType :: TType ty -> SomeType

------------------------------------------------------------
-- Contexts + existential wrappers

data Ctx :: [Type] -> Type where
  CNil :: Ctx '[]
  CCons :: Text -> TType ty -> Ctx g -> Ctx (ty ': g)

data SomeIdx :: [Type] -> Type where
  SomeIdx :: Idx g ty -> TType ty -> SomeIdx g

mapSomeIdx :: (forall ty. Idx g1 ty -> Idx g2 ty) -> SomeIdx g1 -> SomeIdx g2
mapSomeIdx f (SomeIdx i ty) = SomeIdx (f i) ty

lookup :: Text -> Ctx g -> Maybe (SomeIdx g)
lookup _ CNil = Nothing
lookup x (CCons y ty ctx)
  | x == y = Just (SomeIdx VZ ty)
  | otherwise = mapSomeIdx VS <$> lookup x ctx

data SomeTerm :: [Type] -> Type where
  SomeTerm :: TType ty -> TTerm g ty -> SomeTerm g

deriving instance Show (SomeTerm g)

------------------------------------------------------------
-- Type inference/checking + elaboration

check :: Ctx g -> WExp -> TType t -> Maybe (TTerm g t)
check e t ty = do
  t1 <- infer e t
  SomeTerm ty' t' <- apply (SomeTerm (ty :->: ty) (embed I)) t1
  case testEquality ty ty' of
    Nothing -> Nothing
    Just Refl -> Just t'

-- -- For my own sanity I think we might get rid of the rule Int <: Float
-- -- and only allow promoting/lifting to World.  Then 3 will always be
-- -- Int and 3.0 will be Float.

getBaseType :: SomeTerm g -> SomeType
getBaseType (SomeTerm (TTyWorld ty) _) = SomeType ty
getBaseType (SomeTerm ty _) = SomeType ty

-- Application is where we deal with lifting + promotion.
apply :: SomeTerm g -> SomeTerm g -> Maybe (SomeTerm g)
apply (SomeTerm (ty11 :->: ty12) t1) (SomeTerm ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ SomeTerm ty12 (t1 $$ t2)
apply (SomeTerm (TTyWorld ty11 :->: ty12) t1) (SomeTerm ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ SomeTerm ty12 (t1 $$ (K .$ t2))
apply (SomeTerm (ty11 :->: ty12) t1) (SomeTerm (TTyWorld ty2) t2)
  | Just Refl <- testEquality ty11 ty2 = return $ SomeTerm (TTyWorld ty12) (B .$ t1 $$ t2)
apply (SomeTerm (TTyWorld (ty11 :->: ty12)) t1) (SomeTerm ty2 t2)
  | Just Refl <- testEquality ty11 ty2 = return $ SomeTerm (TTyWorld ty12) (S .$ t1 $$ (K .$ t2))
apply (SomeTerm (TTyWorld (ty11 :->: ty12)) t1) (SomeTerm (TTyWorld ty2) t2)
  | Just Refl <- testEquality ty11 ty2 = return $ SomeTerm (TTyWorld ty12) (S .$ t1 $$ t2)
apply _ _ = Nothing

applyTo :: SomeTerm g -> SomeTerm g -> Maybe (SomeTerm g)
applyTo = flip apply

inferOp :: [SomeType] -> Op -> Maybe (SomeTerm g)
inferOp _ Not = return $ SomeTerm (TTyBool :->: TTyBool) (embed CNot)
inferOp [SomeType tyA] Neg = SomeTerm (tyA :->: tyA) <$> checkNum tyA (embed CNeg)
inferOp _ And = return $ SomeTerm (TTyBool :->: TTyBool :->: TTyBool) (embed CAnd)
inferOp _ Or = return $ SomeTerm (TTyBool :->: TTyBool :->: TTyBool) (embed COr)
inferOp [SomeType tyA] Add = SomeTerm (tyA :->: tyA :->: tyA) <$> checkNum tyA (embed CAdd)
inferOp [SomeType tyA] Sub = SomeTerm (tyA :->: tyA :->: tyA) <$> checkNum tyA (embed CSub)
inferOp [SomeType tyA] Mul = SomeTerm (tyA :->: tyA :->: tyA) <$> checkNum tyA (embed CMul)
inferOp [SomeType tyA] Div = case tyA of
  TTyBase BInt -> return $ SomeTerm (tyA :->: tyA :->: tyA) (embed CIDiv)
  TTyBase BFloat -> return $ SomeTerm (tyA :->: tyA :->: tyA) (embed CDiv)
  _ -> Nothing
inferOp [SomeType tyA] Mod = SomeTerm (tyA :->: tyA :->: tyA) <$> checkIntegral tyA (embed CMod)
inferOp [SomeType tyA] Eq = SomeTerm (tyA :->: tyA :->: TTyBool) <$> checkEq tyA (embed CEq)
inferOp [SomeType tyA] Neq = SomeTerm (tyA :->: tyA :->: TTyBool) <$> checkEq tyA (embed CNeq)
inferOp [SomeType tyA] Lt = SomeTerm (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (embed CLt)
inferOp [SomeType tyA] Leq = SomeTerm (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (embed CLeq)
inferOp [SomeType tyA] Gt = SomeTerm (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (embed CGt)
inferOp [SomeType tyA] Geq = SomeTerm (tyA :->: tyA :->: TTyBool) <$> checkOrd tyA (embed CGeq)
inferOp [SomeType tyA] If = return $ SomeTerm (TTyBool :->: tyA :->: tyA :->: tyA) (embed CIf)
inferOp _ Perlin = return $ SomeTerm (TTyInt :->: TTyInt :->: TTyFloat :->: TTyFloat :->: TTyWorld TTyFloat) (embed CPerlin)
inferOp [SomeType tyA] (Reflect r) = return $ SomeTerm (TTyWorld tyA :->: TTyWorld tyA) (embed (CReflect r))
inferOp [SomeType tyA] (Rot r) = return $ SomeTerm (TTyWorld tyA :->: TTyWorld tyA) (embed (CRot r))
inferOp [SomeType tyA] Mask = return $ SomeTerm (TTyWorld TTyBool :->: TTyWorld tyA :->: TTyWorld tyA) (embed CMask)
inferOp _ _ = error "bad call to inferOp!!"

typeArgsFor :: Op -> [SomeTerm g] -> [SomeType]
typeArgsFor op (t : _)
  | op `elem` [Neg, Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Leq, Gt, Geq] = [getBaseType t]
typeArgsFor (Reflect _) (t : _) = [getBaseType t]
typeArgsFor (Rot _) (t : _) = [getBaseType t]
typeArgsFor op (_ : t : _)
  | op `elem` [If, Mask] = [getBaseType t]
typeArgsFor _ _ = []

applyOp :: Ctx g -> ([SomeTerm g] -> [SomeType]) -> Op -> [WExp] -> Maybe (SomeTerm g)
applyOp ctx typeArgs op ts = do
  tts <- mapM (infer ctx) ts
  foldl (\r -> (r >>=) . applyTo) (inferOp (typeArgs tts) op) tts

infer :: Ctx g -> WExp -> Maybe (SomeTerm g)
infer _ (WInt i) = return $ SomeTerm (TTyBase BInt) (embed (CLit i))
infer _ (WFloat f) = return $ SomeTerm (TTyBase BFloat) (embed (CLit f))
infer _ (WBool b) = return $ SomeTerm (TTyBase BBool) (embed (CLit b))
infer _ (WCell c) = undefined
infer ctx (WVar x) = (\(SomeIdx i ty) -> SomeTerm ty (TVar i)) <$> lookup x ctx
infer ctx (WOp op ts) = applyOp ctx (typeArgsFor op) op ts
infer _ WSeed = return $ SomeTerm TTyInt (embed CSeed)
infer _ (WCoord ax) = return $ SomeTerm (TTyWorld TTyInt) (embed (CCoord ax))
infer _ WHash = return $ SomeTerm (TTyWorld TTyInt) (embed CHash)
infer ctx (WLet defs body) = undefined
infer ctx (WOverlay ts) = undefined
infer ctx (WCat ax ts) = undefined
infer ctx (WStruct pal rect) = undefined
