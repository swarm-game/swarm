{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  pattern TyType,
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
  pattern UTyVar',
  pattern UTyVoid,
  pattern UTyUnit,
  pattern UTyInt,
  pattern UTyText,
  pattern UTyDir,
  pattern UTyBool,
  pattern UTyActor,
  pattern UTyKey,
  pattern UTyType,
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
  hasAnyUVars,
  isTopLevelConstructor,
  UnchainableFun (..),

  -- * Polytypes
  ImplicitQuantification (..),
  Poly,
  mkPoly,
  mkQPoly,
  mkTrivPoly,
  unPoly,
  ptVars,
  ptBody,
  Polytype,
  RawPolytype,
  pattern PolyUnit,
  UPolytype,
  quantify,
  absQuantify,
  forgetQ,

  -- * Contexts
  TCtx,
  UCtx,
  TVCtx,

  -- * User type definitions
  TydefInfo (..),
  tydefType,
  tydefArity,
  substTydef,
  ExpandTydefErr (..),
  expandTydef,
  expandTydefs,
  elimTydef,
  TDCtx,
  emptyTDCtx,
  lookupTD,
  lookupTDR,
  addBindingTD,
  withBindingTD,
  withBindingsTD,
  resolveUserTy,

  -- * WHNF
  whnfType,

  -- * The 'WithU' class
  WithU (..),
) where

import Control.Algebra (Has, run)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Reader (Reader, ask, local)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens (Plated (..), makeLenses, rewriteM, view)
import Control.Monad.Free
import Data.Aeson (FromJSON (..), FromJSON1 (..), ToJSON (..), ToJSON1 (..), genericLiftParseJSON, genericLiftToJSON, genericParseJSON, genericToJSON)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix
import Data.Foldable (fold)
import Data.Functor.Classes (Eq1)
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1)
import Data.Kind qualified
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ord.Deriving (deriveOrd1)
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic, Generic1)
import Prettyprinter (align, braces, brackets, concatWith, flatAlt, hsep, pretty, punctuate, softline, (<+>))
import Swarm.Language.Context (Ctx)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Syntax.Import (ImportLoc, ImportPhase (Resolved))
import Swarm.Language.TDVar (TDVar (..), mkTDVar, setVersion)
import Swarm.Language.Var (Var)
import Swarm.Pretty (PrettyPrec (..), pparens, pparens', ppr, prettyBinding)
import Swarm.Util (showT, unsnocNE)
import Swarm.Util.JSON (optionsMinimize, optionsUnwrapUnary)
import Text.Show.Deriving (deriveShow1)
import Witch (into)

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
  | -- | The type of types
    BType
  deriving (Eq, Ord, Show, Bounded, Enum, Data, Generic, Hashable, FromJSON, ToJSON)

baseTyName :: BaseTy -> Text
baseTyName = into @Text . drop 1 . show

instance PrettyPrec BaseTy where
  prettyPrec _ = pretty . drop 1 . show

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
    TCUser TDVar
  deriving (Eq, Ord, Show, Data, Generic, Hashable)

instance ToJSON TyCon where
  toJSON = genericToJSON optionsMinimize

instance FromJSON TyCon where
  parseJSON = genericParseJSON optionsMinimize

instance PrettyPrec TyCon where
  prettyPrec _ = \case
    TCBase b -> ppr b
    TCCmd -> "Cmd"
    TCDelay -> "Delay"
    TCSum -> "Sum"
    TCProd -> "Prod"
    TCFun -> "Fun"
    TCUser t -> ppr t

-- | The arity of a type, /i.e./ the number of type parameters it
--   expects.
newtype Arity = Arity {getArity :: Int}
  deriving stock (Generic, Data)
  deriving newtype (Eq, Ord, Show, Hashable)

instance ToJSON Arity where
  toJSON = genericToJSON optionsUnwrapUnary

instance FromJSON Arity where
  parseJSON = genericParseJSON optionsUnwrapUnary

instance PrettyPrec Arity where
  prettyPrec _ (Arity a) = pretty a

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | Peano naturals, for use as de Bruijn indices in recursive types.
data Nat where
  NZ :: Nat
  NS :: Nat -> Nat
  deriving (Eq, Ord, Show, Data, Generic, Hashable, FromJSON, ToJSON)

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
  | -- | A type variable.  The first Var represents the original name,
    --   and should be ignored except for use in e.g. error messages.
    --   The second Var is the real name of the variable; it may be the
    --   same as the first, or it may be different e.g. if it is a
    --   freshly generated skolem variable.
    TyVarF Var Var
  | -- | Record type.
    TyRcdF (Map Var t)
  | -- | A recursive type variable bound by an enclosing Rec,
    --   represented by a de Bruijn index.
    TyRecVarF Nat
  | -- | Recursive type.  The variable is just a suggestion for a name to use
    --   when pretty-printing; the actual bound variables are represented
    --   via de Bruijn indices.
    TyRecF Var t
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Hashable)

deriveEq1 ''TypeF
deriveOrd1 ''TypeF
deriveShow1 ''TypeF

instance Hashable1 TypeF -- needs the Eq1 instance

instance ToJSON1 TypeF where
  liftToJSON = genericLiftToJSON optionsMinimize

instance FromJSON1 TypeF where
  liftParseJSON = genericLiftParseJSON optionsMinimize

-- | @Type@ is now defined as the fixed point of 'TypeF'.  It would be
--   annoying to manually apply and match against 'Fix' constructors
--   everywhere, so we provide pattern synonyms that allow us to work
--   with 'Type' as if it were defined in a directly recursive way.
type Type = Fix TypeF

instance Plated Type where
  plate = uniplate

newtype IntVar = IntVar Int
  deriving stock (Data, Generic)
  deriving newtype (Show, Eq, Ord, Hashable)

instance PrettyPrec IntVar where
  prettyPrec _ = ppr . mkVarName "u"

-- | 'UType's are like 'Type's, but also contain unification
--   variables.  'UType' is defined via 'Free', which is also a kind
--   of fixed point (in fact, @Free TypeF@ is the /free monad/ over
--   'TypeF').
--
--   Just as with 'Type', we provide a bunch of pattern synonyms for
--   working with 'UType' as if it were defined directly.
type UType = Free TypeF IntVar

-- orphan instance
instance (Eq1 f, Hashable x, Hashable (f (Free f x))) => Hashable (Free f x)

-- | A generic /fold/ for things defined via 'Free' (including, in
--   particular, 'UType').
ucata :: Functor t => (v -> a) -> (t a -> a) -> Free t v -> a
ucata f _ (Pure v) = f v
ucata f g (Free t) = g (fmap (ucata f g) t)

-- | A quick-and-dirty method for turning an 'IntVar' (used internally
--   as a unification variable) into a unique variable name, by
--   appending a number to the given name.
mkVarName :: Text -> IntVar -> Var
mkVarName nm (IntVar v) = T.append nm (showT v)

-- | Get all the free unification variables in a 'UType'.
fuvs :: UType -> Set IntVar
fuvs = ucata S.singleton fold

-- | Check whether a type contains any unification variables at all.
hasAnyUVars :: UType -> Bool
hasAnyUVars = ucata (const True) or

-- | Check whether a type consists of a top-level type constructor
--   immediately applied to unification variables.
isTopLevelConstructor :: UType -> Maybe (TypeF ())
isTopLevelConstructor = \case
  Free (TyRcdF m) | all isPure m -> Just (TyRcdF M.empty)
  UTyConApp c ts | all isPure ts -> Just (TyConF c [])
  _ -> Nothing

isPure :: Free f a -> Bool
isPure (Pure {}) = True
isPure _ = False

-- | For convenience, so we can write /e.g./ @"a"@ instead of @TyVar "a"@.
instance IsString Type where
  fromString = TyVar . into @Text

instance IsString UType where
  fromString = UTyVar . into @Text

--------------------------------------------------
-- Recursive type utilities

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

--------------------------------------------------
-- Pretty-printing machinery for types

-- | Split a function type chain, so that we can pretty print
--   the type parameters aligned on each line when they don't fit.
class UnchainableFun t where
  unchainFun :: t -> NE.NonEmpty t

instance UnchainableFun Type where
  unchainFun (a :->: ty) = a <| unchainFun ty
  unchainFun ty = pure ty

instance UnchainableFun (Free TypeF ty) where
  unchainFun (Free (TyConF TCFun [ty1, ty2])) = ty1 <| unchainFun ty2
  unchainFun ty = pure ty

instance (UnchainableFun t, PrettyPrec t, SubstRec t) => PrettyPrec (TypeF t) where
  prettyPrec p = \case
    TyVarF v _ -> ppr v
    TyRcdF m -> brackets $ hsep (punctuate "," (map prettyBinding (M.assocs m)))
    -- Special cases for type constructors with special syntax.
    -- Always use parentheses around sum and product types, see #1625
    TyConF TCSum [ty1, ty2] ->
      pparens (p > 0) $
        prettyPrec 2 ty1 <+> "+" <+> prettyPrec 2 ty2
    TyConF TCProd [ty1, ty2] ->
      pparens (p > 0) $
        prettyPrec 2 ty1 <+> "*" <+> prettyPrec 2 ty2
    TyConF TCDelay [ty] -> braces $ ppr ty
    TyConF TCFun [ty1, ty2] ->
      let (iniF, lastF) = unsnocNE $ ty1 <| unchainFun ty2
          funs = (prettyPrec 2 <$> iniF) <> [prettyPrec 1 lastF]
          inLine l r = l <+> "->" <+> r
          multiLine l r = l <+> "->" <> softline <> r
       in pparens' (p > 1) . align $
            flatAlt (concatWith multiLine funs) (concatWith inLine funs)
    TyRecF x ty ->
      pparens (p > 0) $
        "rec" <+> ppr x <> "." <+> prettyPrec 0 (substRec (TyVarF x x) ty NZ)
    -- This case shouldn't be possible, since TyRecVar should only occur inside a TyRec,
    -- and pretty-printing the TyRec (above) will substitute a variable name for
    -- any bound TyRecVars before recursing.
    TyRecVarF i -> pretty (show (natToInt i))
    -- Fallthrough cases for type constructor application.  Handles base
    -- types, Cmd, user-defined types, or ill-kinded things like 'Int
    -- Bool'.
    TyConF c [] -> ppr c
    TyConF c tys -> pparens (p > 9) $ ppr c <+> hsep (map (prettyPrec 10) tys)

------------------------------------------------------------
-- Generic folding over type representations
------------------------------------------------------------

-- | Type class for various type representations (e.g. 'Type',
--   'UType') and generic operations we can perform on them.  This
--   helps us avoid code duplication in some cases, by writing a
--   single generic function which works for any 'Typical' instance.
class Typical t where
  -- | Fold a type into a summary value of some type @a@, given an
  --   "empty" value of type @a@ (for use at e.g. Pure nodes in a
  --   UType)
  foldT :: a -> (TypeF a -> a) -> t -> a

  -- | Refold a type into another type.  This is a special case of
  --   'foldT' when we want to produce another type, since then we can
  --   do something more sensible with 'Pure' nodes in a 'UType'
  --   (i.e. preserve them instead of replacing them with a default
  --   value).
  refoldT :: (TypeF t -> t) -> t -> t

  -- | Unroll one level of structure.
  unrollT :: t -> Maybe (TypeF t)

  -- | Roll up one level of structure.
  rollT :: TypeF t -> t

  -- | It should be possible to convert 'Type' to any type-ish thing.
  fromType :: Type -> t

instance Typical Type where
  foldT _ = foldFix
  refoldT = foldFix
  unrollT (Fix t) = Just t
  rollT = Fix
  fromType = id

instance Typical UType where
  foldT e = ucata (const e)
  refoldT = ucata Pure
  unrollT (Free t) = Just t
  unrollT _ = Nothing
  rollT = Free
  fromType = toU

-- | Get all the type variables (/not/ unification variables)
--   contained in a 'Type' or 'UType'.
tyVars :: Typical t => t -> Set Var
tyVars = foldT S.empty (\case TyVarF _ x -> S.singleton x; f -> fold f)

------------------------------------------------------------
-- Polytypes
------------------------------------------------------------

data ImplicitQuantification = Unquantified | Quantified
  deriving (Eq, Ord, Read, Show)

-- | A @Poly q t@ is a universally quantified @t@.  The variables in
--   the list are bound inside the @t@.  For example, the type @forall
--   a. a -> a@ would be represented as @Forall ["a"] (TyFun "a"
--   "a")@.
--
--   The type index @q@ is a phantom type index indicating whether the
--   type has been implicitly quantified.  Immediately after a
--   polytype is parsed it is 'Unquantified' and unsafe to use.  For
--   example, the type @a -> a@ would parse literally as @Forall []
--   (TyFun "a" "a") :: Poly Unquantified Type@, where the type
--   variable @a@ is not in the list of bound variables.  Later, after
--   running through 'quantify', it would become a @Poly Quantified
--   Type@, either @Forall ["a"] (TyFun "a" "a")@ if the type variable
--   is implicitly quantified, or unchanged if the type variable @a@
--   was already in scope.
--
--   The @Poly@ constructor intentionally unexported, so that the
--   only way to create a @Poly Quantified@ is through the 'quantify'
--   function.
data Poly (q :: ImplicitQuantification) t = Forall {_ptVars :: [Var], ptBody :: t}
  deriving (Show, Eq, Functor, Foldable, Traversable, Data, Generic, FromJSON, ToJSON, Hashable)

-- | Create a raw, unquantified @Poly@ value.
mkPoly :: [Var] -> t -> Poly 'Unquantified t
mkPoly = Forall

-- | Create a polytype while performing implicit quantification.
mkQPoly :: Typical t => t -> Poly 'Quantified t
mkQPoly = absQuantify . Forall []

-- | Create a trivial "polytype" with no bound variables.  This is
--   somewhat unsafe --- only use this if you are sure that the polytype
--   you want has no type variables.
mkTrivPoly :: t -> Poly q t
mkTrivPoly = Forall []

-- | Project out the variables and body of a 'Poly' value.  It's only
--   possible to project from a 'Poly Quantified' since the list of
--   variables might be meaningless for a type that has not had
--   implicit quantification applied.
unPoly :: Poly 'Quantified t -> ([Var], t)
unPoly (Forall xs t) = (xs, t)

-- | Project out the variables of a 'Poly Quantified' value.
ptVars :: Poly 'Quantified t -> [Var]
ptVars (Forall xs _) = xs

-- | Forget that a polytype has been properly quantified.
forgetQ :: Poly 'Quantified t -> Poly 'Unquantified t
forgetQ (Forall xs t) = Forall xs t

-- | A regular polytype (without unification variables).  A @Polytype@
--   (as opposed to a @RawPolytype@) is guaranteed to have implicit
--   quantification properly applied, so that all type variables bound
--   by the forall are explicitly listed.
type Polytype = Poly 'Quantified Type

-- | A raw polytype (without unification variables), which corresponds
--   exactly to the way a polytype was written in source code.  In
--   particular there may be type variables which are used in the type
--   but not listed explicitly, which are to be implicitly quantified.
type RawPolytype = Poly 'Unquantified Type

instance PrettyPrec (Poly q Type) where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("∀" : map ppr xs) <> "." <+> ppr t

-- | A polytype with unification variables.
type UPolytype = Poly 'Quantified UType

instance PrettyPrec (Poly q UType) where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("∀" : map ppr xs) <> "." <+> ppr t

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
pattern TyVar x <- Fix (TyVarF _ x)
  where
    TyVar x = Fix (TyVarF x x)

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

pattern TyType :: Type
pattern TyType = TyBase BType

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

pattern TyUser :: TDVar -> [Type] -> Type
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
pattern UTyVar x <- Free (TyVarF _ x)
  where
    UTyVar x = Free (TyVarF x x)

pattern UTyVar' :: Var -> Var -> UType
pattern UTyVar' x y = Free (TyVarF x y)

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

pattern UTyType :: UType
pattern UTyType = UTyBase BType

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

pattern UTyUser :: TDVar -> [UType] -> UType
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
type TCtx = Ctx Var Polytype

-- | A @UCtx@ is a mapping from variables to polytypes with
--   unification variables.  We generally have one of these while we
--   are in the midst of the type inference process.
type UCtx = Ctx Var UPolytype

-- | A @TVCtx@ tracks which type variables are in scope, and what
--   skolem variables were assigned to them.
type TVCtx = Ctx Var UType

------------------------------------------------------------
-- Implicit quantification of polytypes
------------------------------------------------------------

-- | Implicitly quantify any otherwise unbound type variables.
quantify ::
  (Has (Reader TVCtx) sig m, Typical ty) =>
  Poly 'Unquantified ty ->
  m (Poly 'Quantified ty)
quantify (Forall xs ty) = do
  inScope <- ask @TVCtx
  -- Look at all variables which occur in the type but are not
  -- explicitly bound by the forall, and and are not bound in the
  -- context.  Such variables must be implicitly quantified.
  let implicit =
        (tyVars ty `S.difference` S.fromList xs)
          `S.difference` M.keysSet (Ctx.unCtx inScope)
  pure $ Forall (xs ++ S.toList implicit) ty

-- | Absolute implicit quantification, i.e. assume there are no type
--   variables in scope.
absQuantify :: Typical t => Poly 'Unquantified t -> Poly 'Quantified t
absQuantify = run . runReader @TVCtx Ctx.empty . quantify

------------------------------------------------------------
-- Type definitions
------------------------------------------------------------

data TydefInfo = TydefInfo
  { _tydefType :: Polytype
  , _tydefArity :: Arity
  }
  deriving (Eq, Show, Generic, Data, FromJSON, ToJSON, Hashable)

makeLenses ''TydefInfo

-- | A @TDCtx@ is a mapping from user-defined type constructor names
--   to their definitions and arities/kinds.  It also stores the
--   latest version + current module of each name (for any names with
--   more than one version), so we can tell when a type definition has
--   been shadowed.
data TDCtx = TDCtx
  { getTDCtx :: Ctx TDVar TydefInfo
  -- ^ Mapping from fully resolved + versioned TDVars to
  --   corresponding type definition info record
  , getTDResolved :: Map Text (Int, Maybe (ImportLoc Resolved))
  -- ^ Mapping from raw names to the latest in-scope import location
  --   + version number for that name.
  }
  deriving (Eq, Show, Generic, Data, Hashable, ToJSON)

-- | The empty type definition context.
emptyTDCtx :: TDCtx
emptyTDCtx = TDCtx Ctx.empty M.empty

-- | Look up a variable in the type definition context.
lookupTD :: TDVar -> TDCtx -> Maybe TydefInfo
lookupTD x = Ctx.lookup x . getTDCtx

-- | Look up a variable in an ambient type definition context.
lookupTDR :: Has (Reader TDCtx) sig m => TDVar -> m (Maybe TydefInfo)
lookupTDR x = lookupTD x <$> ask

-- | Add a binding of a variable name to a type definition, giving it
--   an appropriate version number if it shadows other variables with
--   the same name.
addBindingTD :: TDVar -> TydefInfo -> TDCtx -> TDCtx
addBindingTD v info (TDCtx tdCtx tdVersions) =
  let x = tdVarName v
      ver' = maybe 0 (succ . fst) $ M.lookup x tdVersions
   in TDCtx (Ctx.addBinding (setVersion ver' v) info tdCtx) (M.insert x (ver', tdModule v) tdVersions)

-- | Locally extend the ambient type definition context with an
--   additional binding, via 'addBindingTD'.
withBindingTD :: Has (Reader TDCtx) sig m => TDVar -> TydefInfo -> m a -> m a
withBindingTD v info = local (addBindingTD v info)

-- | Right-biased union two 'TDCtx' values, under the assumption that
--   they come from different modules.  In particular, none of the
--   variables from the second TDCtx can shadow variables from the
--   same module, so they do not need to be given new version numbers.
--   We can simply union the contexts.
unionTDCtx :: TDCtx -> TDCtx -> TDCtx
unionTDCtx (TDCtx ctx1 res1) (TDCtx ctx2 res2) = TDCtx (ctx1 <> ctx2) (M.union res2 res1)

-- | Locally extend the ambient type definition context with
--   additional bindings.
--
--   Unlike `withBindingTD`, we assume that this does not result in
--   any shadowing within the same module, so we can simply union the
--   contexts.
--
--   XXX currently, when typechecking modules only explicitly imported
--   things are in scope, NOT transitively imported things.  However,
--   this doesn't match what happens at runtime, where everything
--   transitively imported ends up dumped into the context at the
--   point where we suspend.
withBindingsTD :: Has (Reader TDCtx) sig m => TDCtx -> m a -> m a
withBindingsTD tdCtx = local (`unionTDCtx` tdCtx)

-- | Given the name of a variable representing a user-defined type,
--   fill in the version + importloc of the variable of that name
--   currently in scope.
resolveUserTy :: Has (Reader TDCtx) sig m => Text -> m TDVar
resolveUserTy x = do
  tdCtx <- ask
  case M.lookup x (getTDResolved tdCtx) of
    -- If the variable is not found, just return a default version + import loc;
    -- checking for undefined type variables is done elsewhere.
    Nothing -> pure $ mkTDVar 0 Nothing x
    Just (ver, loc) -> pure $ mkTDVar ver loc x

newtype ExpandTydefErr = UnexpandedUserType {getUnexpanded :: TDVar}
  deriving (Eq, Show)

-- | Expand an application "T ty1 ty2 ... tyn" by looking up the
--   definition of T and substituting ty1 .. tyn for its arguments.
--
--   Note that this has already been kind-checked so we know the
--   number of arguments must match up, and user types must be
--   defined; we don't worry about what happens if the lists have
--   different lengths since in theory that can never happen.
--   However, if T does not exist, we throw an error containing its
--   name.
expandTydef ::
  ( Has (Reader TDCtx) sig m
  , Has (Throw ExpandTydefErr) sig m
  , Typical t
  ) =>
  TDVar ->
  [t] ->
  m t
expandTydef userTyCon tys = do
  mtydefInfo <- lookupTDR userTyCon
  case mtydefInfo of
    Nothing -> throwError (UnexpandedUserType userTyCon)
    Just tydefInfo -> pure $ substTydef tydefInfo tys

-- | Expand *all* applications of user-defined type constructors
--   everywhere in a type.
expandTydefs ::
  (Has (Reader TDCtx) sig m, Has (Throw ExpandTydefErr) sig m, Typical t, Plated t) =>
  t ->
  m t
expandTydefs = rewriteM expand
 where
  -- expand :: t -> m (Maybe t)
  expand t = case unrollT t of
    Just (TyConF (TCUser u) tys) -> Just <$> expandTydef u tys
    _ -> pure Nothing

-- | Given the definition of a type alias, substitute the given
--   arguments into its body and return the resulting type.
substTydef :: forall t. Typical t => TydefInfo -> [t] -> t
substTydef (TydefInfo (Forall as ty) _) tys = refoldT @t substF (fromType ty)
 where
  argMap = M.fromList $ zip as tys

  substF tyF@(TyVarF _ x) = case M.lookup x argMap of
    Nothing -> rollT tyF
    Just ty' -> ty'
  substF tyF = rollT tyF

-- | Replace a type alias with its definition everywhere it occurs
--   inside a type.  Typically this is done when reporting the type of
--   a term containing a local tydef: since the tydef is local we
--   can't use it in the reported type.
elimTydef :: forall t. Typical t => TDVar -> TydefInfo -> t -> t
elimTydef x tdInfo = refoldT substF
 where
  substF = \case
    TyConF (TCUser u) tys | u == x -> substTydef tdInfo tys
    tyF -> rollT tyF

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
    TCUser t -> getArity . view tydefArity <$> lookupTD t tydefs

------------------------------------------------------------
-- Reducing types to WHNF
------------------------------------------------------------

-- | Reduce a type to weak head normal form, i.e. keep unfolding type
--   aliases and recursive types just until the top-level constructor
--   of the type is neither @rec@ nor an application of a defined type
--   alias.
whnfType :: TDCtx -> Type -> Type
whnfType tdCtx = run . runReader tdCtx . go
 where
  go :: Has (Reader TDCtx) sig m => Type -> m Type
  go = \case
    TyUser u tys -> do
      res <- runThrow @ExpandTydefErr (expandTydef u tys)
      case res of
        Left _ -> pure $ TyUser u tys
        Right expTy -> go expTy
    TyRec x ty -> go (unfoldRec x ty)
    ty -> pure ty
