-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Types
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for the Swarm programming language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Swarm.Language.Types
  ( BaseTy(..), Var, Ctx, TCtx, UCtx, emptyCtx

  , TypeF(..), Type, UType, Poly(..), Polytype, UPolytype
  , Module(..), TModule, UModule, trivMod

  , WithU(..)

  , pattern TyBase, pattern TyVar
  , pattern TyUnit, pattern TyInt, pattern TyString, pattern TyDir, pattern TyBool
  , pattern (:*:), pattern (:->:)
  , pattern TyCmd

  , pattern UTyBase, pattern UTyVar
  , pattern UTyUnit, pattern UTyInt, pattern UTyString, pattern UTyDir, pattern UTyBool
  , pattern UTyProd, pattern UTyFun
  , pattern UTyCmd

  , ucata, mkVarName
  ) where

import           Control.Unification
import           Control.Unification.IntVar
import           Data.Functor.Fixedpoint
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic1)
import           Witch

data BaseTy
  = BUnit             -- ^ The unit type, with a single inhabitant.
  | BInt              -- ^ Signed, arbitrary-size integers.
  | BString           -- ^ Unicode strings.
  | BDir              -- ^ Directions.
  | BBool             -- ^ Booleans.
  deriving (Eq, Ord, Show)

-- | We use 'Text' values to represent variables.
type Var = Text

-- | A data type representing types in the Swarm programming language.
data TypeF t
  = TyBaseF BaseTy -- ^ Base types.
  | TyVarF Var     -- ^ Type variables.
  | TyCmdF t       -- ^ Commands, with return type.  Note that
                   --   commands form a monad.
  | TyProdF t t    -- ^ Product type.
  | TyFunF t t     -- ^ Function type.
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic1, Unifiable)

-- | A context is a mapping from variable names to things.
type Ctx t = Map Var t
type TCtx = Ctx Polytype
type UCtx = Ctx UPolytype

emptyCtx :: Ctx t
emptyCtx = M.empty

type Type = Fix TypeF
type UType = UTerm TypeF IntVar

instance IsString Type where
  fromString x = TyVar (from @String x)

instance IsString UType where
  fromString x = UTyVar (from @String x)

data Poly t = Forall [Var] t deriving (Show, Eq, Functor)
type Polytype = Poly Type
type UPolytype = Poly UType

data Module s t = Module { moduleTy :: s, moduleCtx :: Ctx t }
  deriving (Show, Eq, Functor)

trivMod :: s -> Module s t
trivMod t = Module t emptyCtx

type TModule = Module Polytype Polytype
type UModule = Module UType UPolytype

class WithU t where
  type U t :: *
  toU   :: t -> U t
  fromU :: U t -> t

instance WithU Type where
  type U Type = UType
  toU = unfreeze
  fromU = fromJust . freeze

instance (WithU t, Functor f) => WithU (f t) where
  type U (f t) = f (U t)
  toU = fmap toU
  fromU = fmap fromU

pattern TyBase :: BaseTy -> Type
pattern TyBase b = Fix (TyBaseF b)

pattern TyVar :: Var -> Type
pattern TyVar v = Fix (TyVarF v)

pattern TyUnit :: Type
pattern TyUnit   = Fix (TyBaseF BUnit)

pattern TyInt :: Type
pattern TyInt    = Fix (TyBaseF BInt)

pattern TyString :: Type
pattern TyString = Fix (TyBaseF BString)

pattern TyDir :: Type
pattern TyDir    = Fix (TyBaseF BDir)

pattern TyBool :: Type
pattern TyBool   = Fix (TyBaseF BBool)

infixl 6 :*:

pattern (:*:) :: Type -> Type -> Type
pattern ty1 :*: ty2 = Fix (TyProdF ty1 ty2)

infixr 1 :->:

pattern (:->:) :: Type -> Type -> Type
pattern ty1 :->: ty2 = Fix (TyFunF ty1 ty2)

pattern TyCmd :: Type -> Type
pattern TyCmd ty1 = Fix (TyCmdF ty1)

pattern UTyBase :: BaseTy -> UType
pattern UTyBase b = UTerm (TyBaseF b)

pattern UTyVar :: Var -> UType
pattern UTyVar v = UTerm (TyVarF v)

pattern UTyUnit :: UType
pattern UTyUnit   = UTerm (TyBaseF BUnit)

pattern UTyInt :: UType
pattern UTyInt    = UTerm (TyBaseF BInt)

pattern UTyString :: UType
pattern UTyString = UTerm (TyBaseF BString)

pattern UTyDir :: UType
pattern UTyDir    = UTerm (TyBaseF BDir)

pattern UTyBool :: UType
pattern UTyBool   = UTerm (TyBaseF BBool)

pattern UTyProd :: UType -> UType -> UType
pattern UTyProd ty1 ty2 = UTerm (TyProdF ty1 ty2)

pattern UTyFun :: UType -> UType -> UType
pattern UTyFun ty1 ty2 = UTerm (TyFunF ty1 ty2)

pattern UTyCmd :: UType -> UType
pattern UTyCmd ty1 = UTerm (TyCmdF ty1)
------------------------------------------------------------
-- Some utilities

ucata :: Functor t => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar v)  = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

mkVarName :: Text -> IntVar -> Var
mkVarName nm (IntVar v) = T.append nm (from @String (show (v + (maxBound :: Int) + 1)))
