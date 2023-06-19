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
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Game.Terrain
import Swarm.Game.Scenario.WorldPalette
import Data.Monoid (Last(..))

------------------------------------------------------------
-- Syntax

data CellVal e r = CellVal (Last TerrainType) (Last e) [r]

instance Semigroup (CellVal e r) where
  CellVal t1 e1 r1 <> CellVal t2 e2 r2 = CellVal (t1 <> t2) (e1 <> e2) (r1 <> r2)

instance Monoid (CellVal e r) where
  mempty = CellVal mempty mempty mempty

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
-- Type checking/elaboration

-- data BaseTy = BInt | BFloat | BBool | BCell
--   deriving (Eq, Ord, Show, Bounded, Enum)

-- data WType = TyBase BaseTy | TyWorld BaseTy | TyPalette BaseTy
--   deriving (Eq, Show)

-- pattern TyInt = TyBase BInt
-- pattern TyFloat = TyBase BFloat
-- pattern TyBool = TyBase BBool
-- pattern TyCell = TyBase BCell

-- type Env = Map Var Type

data TypeErr
  = UnboundVar Var
  | NotWorld
  | Mismatch

data TWIdx :: [Type] -> Type -> Type where
  Z :: TWIdx (ty ': env) ty
  S :: TWIdx env ty -> TWIdx (x ': env) ty

data TWExp :: [Type] -> Type -> Type where
  TWLit :: TWBase b -> b -> TWExp env b
  TWVar :: TWIdx env a -> TWExp env a
  TWOp :: TOp t -> TWExp env t

  TWPromote :: TWExp env t -> TWExp env (World t)

  TWApp :: TWExp env (a -> b) -> TWExp env a -> TWExp env b
  TWLift1 :: TWExp env (a -> b) -> TWExp env (World a -> World b)
  TWLift2 :: TWExp env (a -> b -> c) -> TWExp env (World a -> World b -> World c)

data TOp :: Type -> Type where
  TWNot :: TOp (Bool -> Bool)
  TWNeg :: Num a => TOp (a -> a)
  TWAnd :: TOp (Bool -> Bool -> Bool)
  TWOr :: TOp (Bool -> Bool -> Bool)
  TWAdd :: Num a => TOp (a -> a -> a)

  --Sub | Mul | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq

newtype Coords = Cords {unCoords :: (Int32, Int32)} -- XXX
newtype World b = World { runWorld :: Coords -> b }

data TWBase :: Type -> Type where
  TWBInt :: TWBase Integer
  TWBFloat :: TWBase Double
  TWBBool :: TWBase Bool

data TWType :: Type -> Type where
  TWTyBase :: TWBase t -> TWType t
  TWTyWorld :: TWBase t -> TWType (World t)

data TWEnv :: [Type] -> Type where
  Nil :: TWEnv '[]
  Cons :: t -> TWEnv g -> TWEnv (t : g)

-- inferUOp :: UOp -> TOp 

check :: TWEnv g -> WExp -> TWType t -> Either TypeErr (TWExp g t)
check _ (WInt i) (TWTyBase TWBInt) = Right (TWLit TWBInt i)
check _ (WInt i) (TWTyWorld TWBInt) = Right (TWPromote (TWLit TWBInt i))
check _ (WFloat i) (TWTyBase TWBFloat) = Right (TWLit TWBFloat i)
check _ (WFloat i) (TWTyWorld TWBFloat) = Right (TWPromote (TWLit TWBFloat i))
check _ (WBool i) (TWTyBase TWBBool) = Right (TWLit TWBBool i)
check _ (WBool i) (TWTyWorld TWBBool) = Right (TWPromote (TWLit TWBBool i))

-- check _ (WUn u)

-- infer :: Env -> WExp -> Either TypeErr Type
-- infer _ (WInt _) = return TyInt
-- infer _ (WFloat _) = return TyFloat
-- infer _ (WBool _) = return TyBool
-- infer _ (WCell _) = return TyCell
-- infer env (WVar x) = maybe (Left $ UnboundVar x) Right $ M.lookup x env
-- infer env (WUn uop e) = inferUOp env uop e
-- infer env (WBin bop e1 e2) = inferBOp env bop e1 e2
-- infer env (WMask e1 e2) = do
--   check env e1 (TyWorld BBool)
--   t <- inferWorld env e2
--   return t

-- inferUOp :: Env -> UOp -> WExp -> Either TypeErr Type
-- inferUOp = undefined

-- inferBOp :: Env -> BOp -> WExp -> WExp -> Either TypeErr Type
-- inferBOp = undefined

-- inferWorld :: Env -> WExp -> Either TypeErr Type
-- inferWorld env e = do
--   t <- infer env e
--   case t of
--     TyBase b -> return $ TyWorld b
--     TyWorld b -> return $ TyWorld b
--     TyPalette b -> Left NotWorld

-- check :: Env -> WExp -> Type -> Either TypeErr ()
-- check env e ty = do
--   ty' <- infer env e
--   case isSubtype ty' ty of
--     True -> return ()
--     False -> Left Mismatch

-- isSubtype :: Type -> Type -> Bool
-- isSubtype ty1 ty2 = case (ty1, ty2) of
--   _ | ty1 == ty2 -> True
--   (TyBase b1, TyBase b2) -> isBaseSubtype b1 b2
--   (TyBase b1, TyWorld b2) -> isBaseSubtype b1 b2
--   _ -> False

-- isBaseSubtype :: BaseTy -> BaseTy -> Bool
-- isBaseSubtype b1 b2 = case (b1,b2) of
--   _ | b1 == b2 -> True
--   (BInt, BFloat) -> True
--   _ -> False
