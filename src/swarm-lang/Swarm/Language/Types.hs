{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for the Swarm programming language and related utilities.
module Swarm.Language.Types (
  -- * Basic definitions

  -- ** Base types and type constructors
  BaseTy (..),
  baseTyName,
  TyCon (..),
  Arity (..),
  tcArity,
  Var,

  -- ** Type structure functor
  TypeF (..),

  -- ** Recursive types
  Nat (..),
  natToInt,
  unfoldRec,
  SubstRec (..),

  -- * @Type@
  Type,
  tyVars,
  pattern TyConApp,
  pattern TyBase,
  pattern TyVar,
  pattern TyVoid,
  pattern TyUnit,
  pattern TyInt,
  pattern TyText,
  pattern TyDir,
  pattern TyBool,
  pattern TyActor,
  pattern TyKey,
  pattern (:+:),
  pattern (:*:),
  pattern (:->:),
  pattern TyRcd,
  pattern TyCmd,
  pattern TyDelay,
  pattern TyUser,
  pattern TyRec,

  -- * @UType@
  IntVar (..),
  UType,
  pattern UTyConApp,
  pattern UTyBase,
  pattern UTyVar,
  pattern UTyVoid,
  pattern UTyUnit,
  pattern UTyInt,
  pattern UTyText,
  pattern UTyDir,
  pattern UTyBool,
  pattern UTyActor,
  pattern UTyKey,
  pattern UTySum,
  pattern UTyProd,
  pattern UTyFun,
  pattern UTyRcd,
  pattern UTyCmd,
  pattern UTyDelay,
  pattern UTyUser,
  pattern UTyRec,

  -- ** Utilities
  ucata,
  mkVarName,
  fuvs,

  -- * Polytypes
  Poly (..),
  Polytype,
  pattern PolyUnit,
  UPolytype,

  -- * Contexts
  TCtx,
  UCtx,

  -- * User type definitions
  TydefInfo (..),
  tydefType,
  tydefArity,
  substTydef,
  expandTydef,
  TDCtx,

  -- * The 'WithU' class
  WithU (..),
) where

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask)
import Control.Lens (makeLenses, view)
import Control.Monad.Free
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.TH (defaultOptions, deriveFromJSON1, deriveToJSON1)
import Data.Data (Data)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix
import Data.Foldable (fold)
import Data.Kind qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord.Deriving (deriveOrd1)
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic, Generic1)
import Swarm.Language.Context (Ctx, Var)
import Swarm.Language.Context qualified as Ctx
import Swarm.Util (parens, showT)
import Swarm.Util.JSON (optionsUnwrapUnary)
import Text.Show.Deriving (deriveShow1)
import Witch

------------------------------------------------------------
-- Base types
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
  | -- | Keys, i.e. things that can be pressed on the keyboard
    BKey
  deriving (Eq, Ord, Show, Bounded, Enum, Data, Generic, FromJSON, ToJSON)

baseTyName :: BaseTy -> Text
baseTyName = into @Text . drop 1 . show

------------------------------------------------------------
-- Type constructors
------------------------------------------------------------

-- | Type constructors.
data TyCon
  = -- | Base types are (nullary) type constructors.
    TCBase BaseTy
  | -- | Command types.
    TCCmd
  | -- | Delayed computations.
    TCDelay
  | -- | Sum types.
    TCSum
  | -- | Product types.
    TCProd
  | -- | Function types.
    TCFun
  | -- | User-defined type constructor.
    TCUser Var
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

-- | The arity of a type, /i.e./ the number of type parameters it
--   expects.
newtype Arity = Arity {getArity :: Int}
  deriving (Eq, Ord, Show, Generic, Data)

instance ToJSON Arity where
  toJSON = genericToJSON optionsUnwrapUnary

instance FromJSON Arity where
  parseJSON = genericParseJSON optionsUnwrapUnary

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | Peano naturals, for use as de Bruijn indices in recursive types.
data Nat where
  NZ :: Nat
  NS :: Nat -> Nat
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

natToInt :: Nat -> Int
natToInt NZ = 0
natToInt (NS n) = 1 + natToInt n

-- | A "structure functor" encoding the shape of type expressions.
--   Actual types are then represented by taking a fixed point of this
--   functor.  We represent types in this way, via a "two-level type",
--   so that we can easily use generic recursion schemes to implement
--   things like substitution.
data TypeF t
  = -- | A type constructor applied to some type arguments. For now,
    --   all type constructor applications are required to be fully
    --   saturated (higher kinds are not supported), so we just
    --   directly store a list of all arguments (as opposed to
    --   iterating binary application).
    TyConF TyCon [t]
  | -- | A type variable.
    TyVarF Var
  | -- | Record type.
    TyRcdF (Map Var t)
  | -- | A recursive type variable bound by an enclosing Rec,
    --   represented by a de Bruijn index.
    TyRecVarF Nat
  | -- | Recursive type.  The variable is just a suggestion for a name to use
    --   when pretty-printing; the actual bound variables are represented
    --   via de Bruijn indices.
    TyRecF Var t
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, FromJSON, ToJSON)

deriveEq1 ''TypeF
deriveOrd1 ''TypeF
deriveShow1 ''TypeF
deriveFromJSON1 defaultOptions ''TypeF
deriveToJSON1 defaultOptions ''TypeF

-- | @Type@ is now defined as the fixed point of 'TypeF'.  It would be
--   annoying to manually apply and match against 'Fix' constructors
--   everywhere, so we provide pattern synonyms that allow us to work
--   with 'Type' as if it were defined in a directly recursive way.
type Type = Fix TypeF

-- | Get all the type variables contained in a 'Type'.
tyVars :: Type -> Set Var
tyVars = foldFix (\case TyVarF x -> S.singleton x; f -> fold f)

newtype IntVar = IntVar Int
  deriving (Show, Data, Eq, Ord)

-- | 'UType's are like 'Type's, but also contain unification
--   variables.  'UType' is defined via 'Free', which is also a kind
--   of fixed point (in fact, @Free TypeF@ is the /free monad/ over
--   'TypeF').
--
--   Just as with 'Type', we provide a bunch of pattern synonyms for
--   working with 'UType' as if it were defined directly.
type UType = Free TypeF IntVar

-- | A generic /fold/ for things defined via 'Free' (including, in
--   particular, 'UType').
ucata :: Functor t => (v -> a) -> (t a -> a) -> Free t v -> a
ucata f _ (Pure v) = f v
ucata f g (Free t) = g (fmap (ucata f g) t)

-- | A quick-and-dirty method for turning an 'IntVar' (used internally
--   as a unification variable) into a unique variable name, by
--   appending a number to the given name.
mkVarName :: Text -> IntVar -> Var
mkVarName nm (IntVar v) = T.append nm (from @String (show v))

-- | Get all the free unification variables in a 'UType'.
fuvs :: UType -> Set IntVar
fuvs = ucata S.singleton fold

-- | For convenience, so we can write /e.g./ @"a"@ instead of @TyVar "a"@.
instance IsString Type where
  fromString x = TyVar (from @String x)

instance IsString UType where
  fromString x = UTyVar (from @String x)

------------------------------------------------------------
-- Generic folding over type representations
------------------------------------------------------------

class Typical t where
  foldT :: (TypeF t -> t) -> t -> t
  rollT :: TypeF t -> t
  fromType :: Type -> t

instance Typical Type where
  foldT = foldFix
  rollT = Fix
  fromType = id

instance Typical UType where
  foldT = ucata Pure
  rollT = Free
  fromType = toU

------------------------------------------------------------
-- Polytypes
------------------------------------------------------------

-- | A @Poly t@ is a universally quantified @t@.  The variables in the
--   list are bound inside the @t@.  For example, the type @forall
--   a. a -> a@ would be represented as @Forall ["a"] (TyFun "a" "a")@.
data Poly t = Forall [Var] t
  deriving (Show, Eq, Functor, Foldable, Traversable, Data, Generic, FromJSON, ToJSON)

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
  type U t :: Data.Kind.Type

  -- | Convert from @t@ to its associated "@U@-version".  This
  --   direction is always safe (we simply have no unification
  --   variables even though the type allows it).
  toU :: t -> U t

  -- | Convert from the associated "@U@-version" back to @t@.
  --   Generally, this direction requires somehow knowing that there
  --   are no longer any unification variables in the value being
  --   converted.
  fromU :: U t -> Maybe t

-- | 'Type' is an instance of 'WithU', with associated type 'UType'.
instance WithU Type where
  type U Type = UType
  toU = foldFix Free
  fromU = ucata (const Nothing) (fmap wrapFix . sequence)

-- | A 'WithU' instance can be lifted through any functor (including,
--   in particular, 'Ctx' and 'Poly').
instance (WithU t, Traversable f) => WithU (f t) where
  type U (f t) = f (U t)
  toU = fmap toU
  fromU = traverse fromU

------------------------------------------------------------
-- Pattern synonyms
------------------------------------------------------------

--------------------------------------------------
-- Type

pattern TyConApp :: TyCon -> [Type] -> Type
pattern TyConApp c tys = Fix (TyConF c tys)

pattern TyBase :: BaseTy -> Type
pattern TyBase b = TyConApp (TCBase b) []

pattern TyVar :: Var -> Type
pattern TyVar v = Fix (TyVarF v)

pattern TyVoid :: Type
pattern TyVoid = TyBase BVoid

pattern TyUnit :: Type
pattern TyUnit = TyBase BUnit

pattern TyInt :: Type
pattern TyInt = TyBase BInt

pattern TyText :: Type
pattern TyText = TyBase BText

pattern TyDir :: Type
pattern TyDir = TyBase BDir

pattern TyBool :: Type
pattern TyBool = TyBase BBool

pattern TyActor :: Type
pattern TyActor = TyBase BActor

pattern TyKey :: Type
pattern TyKey = TyBase BKey

infixr 5 :+:

pattern (:+:) :: Type -> Type -> Type
pattern ty1 :+: ty2 = TyConApp TCSum [ty1, ty2]

infixr 6 :*:

pattern (:*:) :: Type -> Type -> Type
pattern ty1 :*: ty2 = TyConApp TCProd [ty1, ty2]

infixr 1 :->:

pattern (:->:) :: Type -> Type -> Type
pattern ty1 :->: ty2 = TyConApp TCFun [ty1, ty2]

pattern TyRcd :: Map Var Type -> Type
pattern TyRcd m = Fix (TyRcdF m)

pattern TyCmd :: Type -> Type
pattern TyCmd ty = TyConApp TCCmd [ty]

pattern TyDelay :: Type -> Type
pattern TyDelay ty = TyConApp TCDelay [ty]

pattern TyUser :: Var -> [Type] -> Type
pattern TyUser v tys = TyConApp (TCUser v) tys

pattern TyRec :: Var -> Type -> Type
pattern TyRec x ty = Fix (TyRecF x ty)

--------------------------------------------------
-- UType

pattern UTyConApp :: TyCon -> [UType] -> UType
pattern UTyConApp c tys = Free (TyConF c tys)

pattern UTyBase :: BaseTy -> UType
pattern UTyBase b = UTyConApp (TCBase b) []

pattern UTyVar :: Var -> UType
pattern UTyVar v = Free (TyVarF v)

pattern UTyVoid :: UType
pattern UTyVoid = UTyBase BVoid

pattern UTyUnit :: UType
pattern UTyUnit = UTyBase BUnit

pattern UTyInt :: UType
pattern UTyInt = UTyBase BInt

pattern UTyText :: UType
pattern UTyText = UTyBase BText

pattern UTyDir :: UType
pattern UTyDir = UTyBase BDir

pattern UTyBool :: UType
pattern UTyBool = UTyBase BBool

pattern UTyActor :: UType
pattern UTyActor = UTyBase BActor

pattern UTyKey :: UType
pattern UTyKey = UTyBase BKey

pattern UTySum :: UType -> UType -> UType
pattern UTySum ty1 ty2 = UTyConApp TCSum [ty1, ty2]

pattern UTyProd :: UType -> UType -> UType
pattern UTyProd ty1 ty2 = UTyConApp TCProd [ty1, ty2]

pattern UTyFun :: UType -> UType -> UType
pattern UTyFun ty1 ty2 = UTyConApp TCFun [ty1, ty2]

pattern UTyRcd :: Map Var UType -> UType
pattern UTyRcd m = Free (TyRcdF m)

pattern UTyCmd :: UType -> UType
pattern UTyCmd ty = UTyConApp TCCmd [ty]

pattern UTyDelay :: UType -> UType
pattern UTyDelay ty = UTyConApp TCDelay [ty]

pattern UTyUser :: Var -> [UType] -> UType
pattern UTyUser v tys = UTyConApp (TCUser v) tys

pattern UTyRec :: Var -> UType -> UType
pattern UTyRec x ty = Free (TyRecF x ty)

pattern PolyUnit :: Polytype
pattern PolyUnit = Forall [] (TyCmd TyUnit)

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
-- Type definitions
------------------------------------------------------------

data TydefInfo = TydefInfo
  { _tydefType :: Polytype
  , _tydefArity :: Arity
  }
  deriving (Eq, Show, Generic, Data, FromJSON, ToJSON)

makeLenses ''TydefInfo

-- | A @TDCtx@ is a mapping from user-defined type constructor names
--   to their definitions and arities/kinds.
type TDCtx = Ctx TydefInfo

-- | Expand an application "T ty1 ty2 ... tyn" by looking up the
--   definition of T and substituting ty1 .. tyn for its arguments.
--
--   Note that this has already been kind-checked so we know the
--   number of arguments must match up; we don't worry about what
--   happens if the lists have different lengths since in theory that
--   can never happen.
expandTydef :: (Has (Reader TDCtx) sig m, Typical t) => Var -> [t] -> m t
expandTydef userTyCon tys = do
  mtydefInfo <- Ctx.lookupR userTyCon
  tdCtx <- ask @TDCtx
  -- In theory, if everything has kind checked, we should never encounter an undefined
  -- type constructor here.
  let errMsg =
        into @String $
          T.unwords
            [ "Encountered undefined type constructor"
            , userTyCon
            , "in expandTyDef"
            , parens ("tdCtx = " <> showT tdCtx)
            ]
      tydefInfo = fromMaybe (error errMsg) mtydefInfo
  return $ substTydef tydefInfo tys

-- | Given the definition of a type alias, substitute the given
--   arguments into its body and return the resulting type.
substTydef :: forall t. Typical t => TydefInfo -> [t] -> t
substTydef (TydefInfo (Forall as ty) _) tys = foldT @t substF (fromType ty)
 where
  argMap = M.fromList $ zip as tys

  substF tyF@(TyVarF x) = case M.lookup x argMap of
    Nothing -> rollT tyF
    Just ty' -> ty'
  substF tyF = rollT tyF

------------------------------------------------------------
-- Arity
------------------------------------------------------------

-- | The arity, /i.e./ number of type arguments, of each type
--   constructor.  Eventually, if we generalize to higher-kinded
--   types, we'll need to upgrade this to return a full-fledged kind
--   instead of just an arity.
tcArity :: TDCtx -> TyCon -> Maybe Arity
tcArity tydefs =
  fmap Arity . \case
    TCBase _ -> Just 0
    TCCmd -> Just 1
    TCDelay -> Just 1
    TCSum -> Just 2
    TCProd -> Just 2
    TCFun -> Just 2
    TCUser t -> getArity . view tydefArity <$> Ctx.lookup t tydefs

------------------------------------------------------------
-- Recursive type utilities
------------------------------------------------------------

-- | @unfoldRec x t@ unfolds the recursive type @rec x. t@ one step,
--   to @t [(rec x. t) / x]@.
unfoldRec :: SubstRec t => Var -> t -> t
unfoldRec x ty = substRec (TyRecF x ty) ty NZ

-- | Class of type-like things where we can substitute for a bound de
--   Bruijn variable.
class SubstRec t where
  -- | @substRec s t n@ substitutes @s@ for the bound de Bruijn variable
  --   @n@ everywhere in @t@.
  substRec :: TypeF t -> t -> Nat -> t

instance SubstRec (Free TypeF v) where
  substRec s = ucata (\i _ -> Pure i) $ \f i -> case f of
    TyRecVarF j
      | i == j -> Free s
      | otherwise -> Free (TyRecVarF j)
    TyRecF x g -> Free (TyRecF x (g (NS i)))
    _ -> Free (fmap ($ i) f)

instance SubstRec Type where
  substRec s = foldFix $ \f i -> case f of
    TyRecVarF j
      | i == j -> Fix s
      | otherwise -> Fix (TyRecVarF j)
    TyRecF x g -> Fix (TyRecF x (g (NS i)))
    _ -> Fix (fmap ($ i) f)
