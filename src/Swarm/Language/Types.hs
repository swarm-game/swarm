-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Types
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for the Swarm programming language and related utilities.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
  -- for the Data IntVar instance

module Swarm.Language.Types
  ( -- * Basic definitions
    BaseTy(..), Var
  , TypeF(..)

    -- * @Type@
  , Type

  , pattern TyBase, pattern TyVar
  , pattern TyUnit, pattern TyInt, pattern TyString, pattern TyDir, pattern TyBool
  , pattern (:*:), pattern (:->:)
  , pattern TyCmd

    -- * @UType@
  , UType
  , pattern UTyBase, pattern UTyVar
  , pattern UTyUnit, pattern UTyInt, pattern UTyString, pattern UTyDir, pattern UTyBool
  , pattern UTyProd, pattern UTyFun
  , pattern UTyCmd

    -- ** Utilities

  , ucata, mkVarName

    -- * Polytypes
  , Poly(..), Polytype, UPolytype

    -- * Contexts
  , TCtx, UCtx

    -- * Modules
  , Module(..), TModule, UModule, trivMod

    -- * The 'WithU' class
  , WithU(..)

  ) where

import           Control.Unification
import           Control.Unification.IntVar
import           Data.Data                  (Data)
import           Data.Functor.Fixedpoint
import           Data.Maybe                 (fromJust)
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic1)
import           Witch

import           Swarm.Language.Context

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | Base types.
data BaseTy
  = BUnit             -- ^ The unit type, with a single inhabitant.
  | BInt              -- ^ Signed, arbitrary-size integers.
  | BString           -- ^ Unicode strings.
  | BDir              -- ^ Directions.
  | BBool             -- ^ Booleans.
  deriving (Eq, Ord, Show, Data)

-- | A "structure functor" encoding the shape of type expressions.
--   Actual types are then represented by taking a fixed point of this
--   functor.  We represent types in this way, via a "two-level type",
--   so that we can work with the @unification-fd@ package (see
--   https://byorgey.wordpress.com/2021/09/08/implementing-hindley-milner-with-the-unification-fd-library/).
data TypeF t
  = TyBaseF BaseTy -- ^ A base type.
  | TyVarF Var     -- ^ A type variable.
  | TyCmdF t       -- ^ Commands, with return type.  Note that
                   --   commands form a monad.
  | TyProdF t t    -- ^ Product type.
  | TyFunF t t     -- ^ Function type.
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic1, Unifiable, Data)

-- | @Type@ is now defined as the fixed point of 'TypeF'.  It would be
--   annoying to manually apply and match against 'Fix' constructors
--   everywhere, so we provide pattern synonyms that allow us to work
--   with 'Type' as if it were defined in a directly recursive way.
type Type = Fix TypeF

-- The derived Data instance is so we can make a quasiquoter for types.
deriving instance Data Type

-- | 'UType's are like 'Type's, but also contain unification
--   variables.  'UType' is defined via 'UTerm', which is also a kind
--   of fixed point (in fact, 'UType' is the /free monad/ over 'TypeF').
--
--   Just as with 'Type', we provide a bunch of pattern synonyms for
--   working with 'UType' as if it were defined directly.
type UType = UTerm TypeF IntVar

-- The derived Data instances are so we can make a quasiquoter for
-- types.
deriving instance Data UType
deriving instance Data IntVar

-- | A generic /fold/ for things defined via 'UTerm' (including, in
--   particular, 'UType').  This probably belongs in the
--   @unification-fd@ package, but since it doesn't provide one, we
--   define it here.
ucata :: Functor t => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar v)  = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

-- | A quick-and-dirty method for turning an 'IntVar' (used internally
--   as a unification variable) into a unique variable name, by
--   appending a number to the given name.
mkVarName :: Text -> IntVar -> Var
mkVarName nm (IntVar v) = T.append nm (from @String (show (v + (maxBound :: Int) + 1)))

-- | For convenience, so we can write /e.g./ @"a"@ instead of @TyVar "a"@.
instance IsString Type where
  fromString x = TyVar (from @String x)

instance IsString UType where
  fromString x = UTyVar (from @String x)

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | A @TCtx@ is a mapping from variables to polytypes.  We generally
--   get one of these at the end of the type inference process.
type TCtx = Ctx Polytype

-- | A @UCtx@ is a mapping from variables to polytypes with
--   unification variables.  We generally have one of these while we
--   are in the midst of the type inference process.
type UCtx = Ctx UPolytype

------------------------------------------------------------
-- Polytypes
------------------------------------------------------------

-- | A @Poly t@ is a universally quantified @t@.  The variables in the
--   list are bound inside the @t@.  For example, the type @forall
--   a. a -> a@ would be represented as @Forall ["a"] (TyFun "a" "a")@.
data Poly t = Forall [Var] t deriving (Show, Eq, Functor, Data)

-- | A polytype without unification variables.
type Polytype = Poly Type

-- | A polytype with unification variables.
type UPolytype = Poly UType

------------------------------------------------------------
-- Modules
------------------------------------------------------------

-- | A module generally represents the result of performing type
--   inference on a top-level expression, which in particular can
--   contain definitions ('Swarm.Language.Syntax.TDef').  A module
--   contains the overall type of the expression, as well as the
--   context giving the types of any defined variables.
data Module s t = Module { moduleTy :: s, moduleCtx :: Ctx t }
  deriving (Show, Eq, Functor, Data)

-- | A 'TModule' is the final result of the type inference process on
--   an expression: we get a polytype for the expression, and a
--   context of polytypes for the defined variables.
type TModule = Module Polytype Polytype

-- | A 'UModule' represents the type of an expression at some
--   intermediate stage during the type inference process.  We get a
--   'UType' (/not/ a 'UPolytype') for the expression, which may
--   contain some free unification or type variables, as well as a
--   context of 'UPolytype's for any defined variables.
type UModule = Module UType UPolytype

-- | The trivial module for a given @s@, with the empty context.
trivMod :: s -> Module s t
trivMod t = Module t empty

------------------------------------------------------------
-- WithU
------------------------------------------------------------

-- | In several cases we have two versions of something: a "normal"
--   version, and a @U@ version with unification variables in it
--   (/e.g./ 'Type' vs 'UType', 'Polytype' vs 'UPolytype', 'TCtx' vs
--   'UCtx'). This class abstracts over the process of converting back
--   and forth between them.
--
--   In particular, @'WithU' t@ represents the fact that the type @t@
--   also has a @U@ counterpart, with a way to convert back and forth.
--   Note, however, that converting back may be "unsafe" in the sense
--   that it requires an extra burden of proof to guarantee that it is
--   used only on inputs that are safe.
class WithU t where
  -- | The associated "@U@-version" of the type @t@.
  type U t :: *

  -- | Convert from @t@ to its associated "@U@-version".  This
  --   direction is always safe (we simply have no unification
  --   variables even though the type allows it).
  toU   :: t -> U t

  -- | Convert from the associated "@U@-version" back to @t@.
  --   Generally, this direction requires somehow knowing that there
  --   are no longer any unification variables in the value being
  --   converted.
  fromU :: U t -> t

-- | 'Type' is an instance of 'WithU', with associated type 'UType'.
instance WithU Type where
  type U Type = UType
  toU = unfreeze
  fromU = fromJust . freeze

-- | A 'WithU' instance can be lifted through any functor (including,
--   in particular, 'Ctx' and 'Poly').
instance (WithU t, Functor f) => WithU (f t) where
  type U (f t) = f (U t)
  toU = fmap toU
  fromU = fmap fromU

------------------------------------------------------------
-- Pattern synonyms
------------------------------------------------------------

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
