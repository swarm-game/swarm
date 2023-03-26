{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- for the Data IntVar instance

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for the Swarm programming language and related utilities.
module Swarm.Language.Types (
  -- * Basic definitions
  BaseTy (..),
  Var,
  TypeF (..),

  -- * @Type@
  Type,
  tyVars,
  pattern TyBase,
  pattern TyVar,
  pattern TyVoid,
  pattern TyUnit,
  pattern TyInt,
  pattern TyText,
  pattern TyDir,
  pattern TyBool,
  pattern TyActor,
  pattern (:+:),
  pattern (:*:),
  pattern (:->:),
  pattern TyRcd,
  pattern TyCmd,
  pattern TyDelay,

  -- * @UType@
  UType,
  pattern UTyBase,
  pattern UTyVar,
  pattern UTyVoid,
  pattern UTyUnit,
  pattern UTyInt,
  pattern UTyText,
  pattern UTyDir,
  pattern UTyBool,
  pattern UTyActor,
  pattern UTySum,
  pattern UTyProd,
  pattern UTyFun,
  pattern UTyRcd,
  pattern UTyCmd,
  pattern UTyDelay,

  -- ** Utilities
  ucata,
  mkVarName,

  -- * Polytypes
  Poly (..),
  Polytype,
  pattern PolyUnit,
  UPolytype,

  -- * Contexts
  TCtx,
  UCtx,

  -- * The 'WithU' class
  WithU (..),
) where

import Control.Monad (guard)
import Control.Unification
import Control.Unification.IntVar
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor.Fixedpoint
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic, Generic1)
import Swarm.Language.Context
import Witch

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | Base types.
data BaseTy
  = -- | The void type, with no inhabitants.
    BVoid
  | -- | The unit type, with a single inhabitant.
    BUnit
  | -- | Signed, arbitrary-size integers.
    BInt
  | -- | Unicode strings.
    BText
  | -- | Directions.
    BDir
  | -- | Booleans.
    BBool
  | -- | "Actors", i.e. anything that can do stuff. Internally, these
    --   are all just "robots", but we give them a more generic
    --   in-game name because they could represent other things like
    --   aliens, animals, seeds, ...
    BActor
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

-- | A "structure functor" encoding the shape of type expressions.
--   Actual types are then represented by taking a fixed point of this
--   functor.  We represent types in this way, via a "two-level type",
--   so that we can work with the @unification-fd@ package (see
--   https://byorgey.wordpress.com/2021/09/08/implementing-hindley-milner-with-the-unification-fd-library/).
data TypeF t
  = -- | A base type.
    TyBaseF BaseTy
  | -- | A type variable.
    TyVarF Var
  | -- | Commands, with return type.  Note that
    --   commands form a monad.
    TyCmdF t
  | -- | Type of delayed computations.
    TyDelayF t
  | -- | Sum type.
    TySumF t t
  | -- | Product type.
    TyProdF t t
  | -- | Function type.
    TyFunF t t
  | -- | Record type.
    TyRcdF (Map Var t)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Generic1, Unifiable, Data, FromJSON, ToJSON)

-- | Unify two Maps by insisting they must have exactly the same keys,
--   and if so, simply matching up corresponding values to be
--   recursively unified.  There could be other reasonable
--   implementations, but in our case we will use this for unifying
--   record types, and we do not have any subtyping, so record types
--   will only unify if they have exactly the same keys.
instance Ord k => Unifiable (Map k) where
  zipMatch m1 m2 = do
    guard $ ((==) `on` M.keysSet) m1 m2
    pure $ M.merge M.dropMissing M.dropMissing (M.zipWithMatched (\_ a1 a2 -> Right (a1, a2))) m1 m2

-- | @Type@ is now defined as the fixed point of 'TypeF'.  It would be
--   annoying to manually apply and match against 'Fix' constructors
--   everywhere, so we provide pattern synonyms that allow us to work
--   with 'Type' as if it were defined in a directly recursive way.
type Type = Fix TypeF

-- | Get all the type variables contained in a 'Type'.
tyVars :: Type -> Set Var
tyVars = cata (\case TyVarF x -> S.singleton x; f -> fold f)

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
ucata f _ (UVar v) = f v
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
data Poly t = Forall [Var] t deriving (Show, Eq, Functor, Data, Generic, FromJSON, ToJSON)

-- | A polytype without unification variables.
type Polytype = Poly Type

-- | A polytype with unification variables.
type UPolytype = Poly UType

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
  toU :: t -> U t

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

pattern TyVoid :: Type
pattern TyVoid = Fix (TyBaseF BVoid)

pattern TyUnit :: Type
pattern TyUnit = Fix (TyBaseF BUnit)

pattern TyInt :: Type
pattern TyInt = Fix (TyBaseF BInt)

pattern TyText :: Type
pattern TyText = Fix (TyBaseF BText)

pattern TyDir :: Type
pattern TyDir = Fix (TyBaseF BDir)

pattern TyBool :: Type
pattern TyBool = Fix (TyBaseF BBool)

pattern TyActor :: Type
pattern TyActor = Fix (TyBaseF BActor)

infixr 5 :+:

pattern (:+:) :: Type -> Type -> Type
pattern ty1 :+: ty2 = Fix (TySumF ty1 ty2)

infixr 6 :*:

pattern (:*:) :: Type -> Type -> Type
pattern ty1 :*: ty2 = Fix (TyProdF ty1 ty2)

infixr 1 :->:

pattern (:->:) :: Type -> Type -> Type
pattern ty1 :->: ty2 = Fix (TyFunF ty1 ty2)

pattern TyRcd :: Map Var Type -> Type
pattern TyRcd m = Fix (TyRcdF m)

pattern TyCmd :: Type -> Type
pattern TyCmd ty1 = Fix (TyCmdF ty1)

pattern TyDelay :: Type -> Type
pattern TyDelay ty1 = Fix (TyDelayF ty1)

pattern UTyBase :: BaseTy -> UType
pattern UTyBase b = UTerm (TyBaseF b)

pattern UTyVar :: Var -> UType
pattern UTyVar v = UTerm (TyVarF v)

pattern UTyVoid :: UType
pattern UTyVoid = UTerm (TyBaseF BVoid)

pattern UTyUnit :: UType
pattern UTyUnit = UTerm (TyBaseF BUnit)

pattern UTyInt :: UType
pattern UTyInt = UTerm (TyBaseF BInt)

pattern UTyText :: UType
pattern UTyText = UTerm (TyBaseF BText)

pattern UTyDir :: UType
pattern UTyDir = UTerm (TyBaseF BDir)

pattern UTyBool :: UType
pattern UTyBool = UTerm (TyBaseF BBool)

pattern UTyActor :: UType
pattern UTyActor = UTerm (TyBaseF BActor)

pattern UTySum :: UType -> UType -> UType
pattern UTySum ty1 ty2 = UTerm (TySumF ty1 ty2)

pattern UTyProd :: UType -> UType -> UType
pattern UTyProd ty1 ty2 = UTerm (TyProdF ty1 ty2)

pattern UTyFun :: UType -> UType -> UType
pattern UTyFun ty1 ty2 = UTerm (TyFunF ty1 ty2)

pattern UTyRcd :: Map Var UType -> UType
pattern UTyRcd m = UTerm (TyRcdF m)

pattern UTyCmd :: UType -> UType
pattern UTyCmd ty1 = UTerm (TyCmdF ty1)

pattern UTyDelay :: UType -> UType
pattern UTyDelay ty1 = UTerm (TyDelayF ty1)

pattern PolyUnit :: Polytype
pattern PolyUnit = Forall [] (TyCmd TyUnit)

-- Derive aeson instances for type serialization
deriving instance Generic Type
deriving instance ToJSON Type
deriving instance FromJSON Type
