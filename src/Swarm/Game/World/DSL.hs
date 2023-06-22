-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Swarm.Game.World.DSL where

import Data.Int
import Data.Kind (Type)
import Data.Monoid (Last(..))
import Data.Text (Text)
import Data.Typeable
import Data.Type.Equality
import Prelude hiding (lookup)
import Swarm.Game.Terrain
import Swarm.Game.Scenario.WorldPalette

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
  WUn :: UOp -> WExp -> WExp
  WBin :: BOp -> WExp -> WExp -> WExp
  WMask :: WExp -> WExp -> WExp
  WSeed :: WExp
  WCoord :: Axis -> WExp
  WPerlin :: WExp -> WExp -> WExp -> WExp -> WExp
  WHash :: WExp
  WIf :: WExp -> WExp -> WExp
  WLet :: [(Var, WExp)] -> WExp -> WExp
  WRot :: Rot -> WExp -> WExp
  WReflect :: Reflection -> WExp -> WExp
  WOverlay :: [WExp] -> WExp
  WCat :: Axis -> [WExp] -> WExp
  WStruct :: WorldPalette Text -> [Text] -> WExp

data UOp = Not | Neg
data BOp = And | Or | Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq

------------------------------------------------------------
-- Example

testWorld1 :: WExp
testWorld1 =
  WLet
    [ ("pn1", WPerlin (WInt 0) (WInt 5) (WFloat 0.05) (WFloat 0.5))
    , ("pn2", WPerlin (WInt 0) (WInt 5) (WFloat 0.05) (WFloat 0.75))
    ]
  $
  WOverlay
    [ WCell (CellVal (Last (Just GrassT)) (Last Nothing) [])
    , WMask (WBin Gt (WVar "pn2") (WFloat 0)) (WCell (CellVal (Last (Just StoneT)) (Last (Just "rock")) []))
    , WMask (WBin Gt (WVar "pn1") (WFloat 0)) (WCell (CellVal (Last (Just DirtT)) (Last (Just "tree")) []))
    , WMask (WBin And (WBin Eq (WCoord X) (WInt 2)) (WBin Eq (WCoord Y) (WInt (-1))))
        (WCell (CellVal (Last (Just GrassT)) (Last (Just "elephant")) []))
    , WMask (WBin And (WBin Eq (WCoord X) (WInt (-5))) (WBin Eq (WCoord Y) (WInt 3)))
        (WCell (CellVal (Last (Just StoneT)) (Last (Just "flerb")) []))
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
  CLit :: Show a => a -> Const a
  CFI  :: Const (Integer -> Double)
  CIf :: Const (Bool -> a -> a -> a)
  CNot :: Const (Bool -> Bool)
  CNeg :: Num a => Const (a -> a)
  CAnd :: Const (Bool -> Bool -> Bool)
  COr :: Const (Bool -> Bool -> Bool)
  CAdd :: Num a => Const (a -> a -> a)
  CSub :: Num a => Const (a -> a -> a)
  CMul :: Num a => Const (a -> a -> a)
  CDiv :: Fractional a => Const (a -> a -> a)
  CIDiv :: Integral a => Const (a -> a -> a)
  CMod :: Integral a => Const (a -> a -> a)
  CEq :: Eq a => Const (a -> a -> Bool)
  CNeq :: Eq a => Const (a -> a -> Bool)
  CLt :: Ord a => Const (a -> a -> Bool)
  CLeq :: Ord a => Const (a -> a -> Bool)
  CGt :: Ord a => Const (a -> a -> Bool)
  CGeq :: Ord a => Const (a -> a -> Bool)
  CMask :: Const (World Bool -> World a -> World a)  -- XXX make our own Empty + Combining classes
  CSeed :: Const Integer
  CCoord :: Axis -> Const (World Integer)

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
  CSeed -> 0  -- XXX need seed provided as env to be able to interpret this
  CCoord ax -> undefined -- \(Coords (x,y)) -> case ax of X -> x; Y -> y  -- XXX Integer vs Int32

  K -> const
  S -> (<*>)
  I -> id
  B -> (.)
  C -> flip

class HasConst t where
  injConst :: Const a -> t a

infixl 1 .$
(.$) :: (HasConst t, Applicable t) => Const (a -> b) -> t a -> t b
c .$ t = injConst c $$ t

infixl 1 $.
($.) :: (HasConst t, Applicable t) => t (a -> b) -> Const a -> t b
t $. c = t $$ injConst c

infixl 1 .$.
(.$.) :: (HasConst t, Applicable t) => Const (a -> b) -> Const a -> t b
c1 .$. c2 = injConst c1 $$ injConst c2

------------------------------------------------------------
-- Intrinsically typed core language

-- Typed de Bruijn indices.
data Idx :: [Type] -> Type -> Type where
  VZ :: Idx (ty ': g) ty
  VS :: Idx g ty -> Idx (x ': g) ty

deriving instance Show (Idx g ty)

-- Type-indexed terms.  Note this is a stripped-down core language,
-- with only variables, lambdas, application, and constants.
data TWTerm :: [Type] -> Type -> Type where
  TWVar :: Idx g a -> TWTerm g a
  TWLam :: TWTerm (ty1 ': g) ty2 -> TWTerm g (ty1 -> ty2)
  TWApp :: TWTerm g (a -> b) -> TWTerm g a -> TWTerm g b
  TWConst :: Const a -> TWTerm g a

deriving instance Show (TWTerm g ty)

instance Applicable (TWTerm g) where
  TWConst I $$ x = x
  f $$ x = TWApp f x

instance HasConst (TWTerm g) where
  injConst = TWConst

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

data TWType :: Type -> Type where
  TWTyBase :: Base t -> TWType t
  TWTyWorld :: Base t -> TWType (World t)
  -- (:->:) :: TWType a -> TWType b -> TWType (a -> b)

pattern TWTyBool = TWTyBase BBool
pattern TWTyInt = TWTyBase BInt
pattern TWTyFloat = TWTyBase BFloat

deriving instance Show (TWType ty)

instance TestEquality TWType where
  testEquality (TWTyBase b1) (TWTyBase b2) = testEquality b1 b2
  testEquality (TWTyWorld b1) (TWTyWorld b2) =
    case testEquality b1 b2 of
      Just Refl -> Just Refl
      Nothing -> Nothing
  testEquality _ _ = Nothing

checkEq :: TWType ty -> (Eq ty => a) -> Maybe a
checkEq _ _ = undefined

checkOrd :: TWType ty -> (Ord ty => a) -> Maybe a
checkOrd _ _ = undefined

------------------------------------------------------------
-- Contexts + existential wrappers

data Ctx :: [Type] -> Type where
  CNil :: Ctx '[]
  CCons :: Text -> TWType ty -> Ctx g -> Ctx (ty ': g)

data SomeIdx :: [Type] -> Type where
  SomeIdx :: Idx g ty -> TWType ty -> SomeIdx g

mapSomeIdx :: (forall ty. Idx g1 ty -> Idx g2 ty) -> SomeIdx g1 -> SomeIdx g2
mapSomeIdx f (SomeIdx i ty) = SomeIdx (f i) ty

lookup :: Text -> Ctx g -> Maybe (SomeIdx g)
lookup _ CNil = Nothing
lookup x (CCons y ty ctx)
  | x == y = Just (SomeIdx VZ ty)
  | otherwise = mapSomeIdx VS <$> lookup x ctx

data SomeTerm :: [Type] -> Type where
  SomeTerm :: TWType ty -> TWTerm g ty -> SomeTerm g

deriving instance Show (SomeTerm g)

------------------------------------------------------------
-- Type inference/checking + elaboration

check :: Ctx g -> WExp -> TWType t -> Maybe (TWTerm g t)
check e t ty = infer e t >>= checkSubtype ty

-- -- For my own sanity I think we might get rid of the rule Int <: Float
-- -- and only allow promoting/lifting to World.  Then 3 will always be
-- -- Int and 3.0 will be Float.

checkSubtype :: TWType t -> SomeTerm g -> Maybe (TWTerm g t)
checkSubtype (TWTyWorld b1) (SomeTerm (TWTyBase b2) t) = do
  case testEquality b1 b2 of
    Just Refl -> return (K .$ t)
    Nothing -> Nothing
checkSubtype ty1 (SomeTerm ty2 t) = do
  case testEquality ty1 ty2 of
    Just Refl -> return t
    Nothing -> Nothing

infer :: Ctx g -> WExp -> Maybe (SomeTerm g)
infer _ (WInt i) = return $ SomeTerm (TWTyBase BInt) (TWConst (CLit i))
infer _ (WFloat f) = return $ SomeTerm (TWTyBase BFloat) (TWConst (CLit f))
infer _ (WBool b) = return $ SomeTerm (TWTyBase BBool) (TWConst (CLit b))
infer ctx (WVar x) = (\(SomeIdx i ty) -> SomeTerm ty (TWVar i)) <$> lookup x ctx
infer ctx (WUn uop t) = infer ctx t >>= applyUOp uop
infer ctx (WBin bop t1 t2) = do
  t1' <- infer ctx t1
  t2' <- infer ctx t2
  applyBOp bop t1' t2'
infer ctx (WMask t1 t2) = do
  t1' <- check ctx t1 (TWTyWorld BBool)
  SomeWorld b t2' <- inferWorld ctx t2
  return $ SomeTerm (TWTyWorld b) (CMask .$ t1' $$ t2')
infer _ WSeed = return $ SomeTerm TWTyInt (injConst CSeed)
infer _ (WCoord ax) = return $ SomeTerm (TWTyWorld BInt) (injConst (CCoord ax))

applyUOp :: UOp -> SomeTerm g -> Maybe (SomeTerm g)
applyUOp = undefined
-- applyUOp Not t = apply (TWTyBool :->: TWTyBool) Not t
-- applyUOp Neg t = apply (TWTyInt :->: TWTyInt)



applyBOp :: BOp -> SomeTerm g -> SomeTerm g -> Maybe (SomeTerm g)
applyBOp = undefined

-- Infer the type of a term which must be of the form (World b).
inferWorld :: Ctx g -> WExp -> Maybe (SomeWorld g)
inferWorld ctx t = do
  SomeTerm ty t' <- infer ctx t
  case ty of
    TWTyWorld b -> return $ SomeWorld b t'
    TWTyBase b -> return $ SomeWorld b (K .$ t')

data SomeWorld :: [Type] -> Type where
  SomeWorld :: Base b -> TWTerm g (World b) -> SomeWorld g

deriving instance Show (SomeWorld g)


-- ------------------------------------------------------------

-- interp :: TWEnv g -> TWTerm g ty -> ty
-- interp _ (TWLit b) = b
-- interp e (TWVar x) = e ! x
-- interp _ (TWOp op) = interpTOp op
-- interp e (TWApp t1 t2) = interp e t1 (interp e t2)
-- interp _ TWFromInt = fromIntegral
-- interp _ TWPure = pure
-- interp _ TWMap  = fmap
-- interp _ TWAp   = (<*>)

-- interpTOp :: TOp t -> t
-- interpTOp TWNot = not
-- interpTOp TWNeg = negate
-- interpTOp TWAnd = (&&)
-- interpTOp TWOr = (||)
-- interpTOp TWAdd = (+)

-- -- check _ (WUn u)

-- -- infer :: Env -> WExp -> Either TypeErr Type
-- -- infer _ (WInt _) = return TyInt
-- -- infer _ (WFloat _) = return TyFloat
-- -- infer _ (WBool _) = return TyBool
-- -- infer _ (WCell _) = return TyCell
-- -- infer env (WVar x) = maybe (Left $ UnboundVar x) Right $ M.lookup x env
-- -- infer env (WUn uop e) = inferUOp env uop e
-- -- infer env (WBin bop e1 e2) = inferBOp env bop e1 e2
-- -- infer env (WMask e1 e2) = do
-- --   check env e1 (TyWorld BBool)
-- --   t <- inferWorld env e2
-- --   return t

-- -- inferUOp :: Env -> UOp -> WExp -> Either TypeErr Type
-- -- inferUOp = undefined

-- -- inferBOp :: Env -> BOp -> WExp -> WExp -> Either TypeErr Type
-- -- inferBOp = undefined

-- -- inferWorld :: Env -> WExp -> Either TypeErr Type
-- -- inferWorld env e = do
-- --   t <- infer env e
-- --   case t of
-- --     TyBase b -> return $ TyWorld b
-- --     TyWorld b -> return $ TyWorld b
-- --     TyPalette b -> Left NotWorld

-- -- check :: Env -> WExp -> Type -> Either TypeErr ()
-- -- check env e ty = do
-- --   ty' <- infer env e
-- --   case isSubtype ty' ty of
-- --     True -> return ()
-- --     False -> Left Mismatch

-- -- isSubtype :: Type -> Type -> Bool
-- -- isSubtype ty1 ty2 = case (ty1, ty2) of
-- --   _ | ty1 == ty2 -> True
-- --   (TyBase b1, TyBase b2) -> isBaseSubtype b1 b2
-- --   (TyBase b1, TyWorld b2) -> isBaseSubtype b1 b2
-- --   _ -> False

-- -- isBaseSubtype :: BaseTy -> BaseTy -> Bool
-- -- isBaseSubtype b1 b2 = case (b1,b2) of
-- --   _ | b1 == b2 -> True
-- --   (BInt, BFloat) -> True
-- --   _ -> False
