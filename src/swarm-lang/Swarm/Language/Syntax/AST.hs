{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types represeting the surface syntax and terms for Swarm programming language.
module Swarm.Language.Syntax.AST (
  Syntax' (..),
  Term' (..),
  DelayType (..),
) where

import Control.Lens (Plated (..))
import Data.Aeson.Types hiding (Key)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.Map.Strict (Map)
import Data.Text hiding (filter, length, map)
import GHC.Generics (Generic)
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Direction
import Swarm.Language.Syntax.Loc
import Swarm.Language.Types

------------------------------------------------------------
-- Syntax: annotation on top of Terms with SrcLoc, comments, + type
------------------------------------------------------------

-- | The surface syntax for the language, with location and type annotations.
data Syntax' ty = Syntax'
  { _sLoc :: SrcLoc
  , _sTerm :: Term' ty
  , _sComments :: Comments
  , _sType :: ty
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic, FromJSON, ToJSON)

instance Data ty => Plated (Syntax' ty) where
  plate = uniplate

-- | Terms of the Swarm language.
data Term' ty
  = -- | The unit value.
    TUnit
  | -- | A constant.
    TConst Const
  | -- | A direction literal.
    TDir Direction
  | -- | An integer literal.
    TInt Integer
  | -- | An antiquoted Haskell variable name of type Integer.
    TAntiInt Text
  | -- | A text literal.
    TText Text
  | -- | An antiquoted Haskell variable name of type Text.
    TAntiText Text
  | -- | A Boolean literal.
    TBool Bool
  | -- | A robot reference.  These never show up in surface syntax, but are
    --   here so we can factor pretty-printing for Values through
    --   pretty-printing for Terms.
    TRobot Int
  | -- | A memory reference.  These likewise never show up in surface syntax,
    --   but are here to facilitate pretty-printing.
    TRef Int
  | -- | Require a specific device to be installed.
    TRequireDevice Text
  | -- | Require a certain number of an entity.
    TRequire Int Text
  | -- | Primitive command to log requirements of a term.  The Text
    --   field is to store the unaltered original text of the term, for use
    --   in displaying the log message (since once we get to execution time the
    --   original term may have been elaborated, e.g. `force` may have been added
    --   around some variables, etc.)
    SRequirements Text (Syntax' ty)
  | -- | A variable.
    TVar Var
  | -- | A pair.
    SPair (Syntax' ty) (Syntax' ty)
  | -- | A lambda expression, with or without a type annotation on the
    --   binder.
    SLam LocVar (Maybe Type) (Syntax' ty)
  | -- | Function application.
    SApp (Syntax' ty) (Syntax' ty)
  | -- | A (recursive) let expression, with or without a type
    --   annotation on the variable. The @Bool@ indicates whether
    --   it is known to be recursive.
    SLet Bool LocVar (Maybe Polytype) (Syntax' ty) (Syntax' ty)
  | -- | A (recursive) definition command, which binds a variable to a
    --   value in subsequent commands. The @Bool@ indicates whether the
    --   definition is known to be recursive.
    SDef Bool LocVar (Maybe Polytype) (Syntax' ty)
  | -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    SBind (Maybe LocVar) (Syntax' ty) (Syntax' ty)
  | -- | Delay evaluation of a term, written @{...}@.  Swarm is an
    --   eager language, but in some cases (e.g. for @if@ statements
    --   and recursive bindings) we need to delay evaluation.  The
    --   counterpart to @{...}@ is @force@, where @force {t} = t@.
    --   Note that 'Force' is just a constant, whereas 'SDelay' has to
    --   be a special syntactic form so its argument can get special
    --   treatment during evaluation.
    SDelay DelayType (Syntax' ty)
  | -- | Record literals @[x1 = e1, x2 = e2, x3, ...]@ Names @x@
    --   without an accompanying definition are sugar for writing
    --   @x=x@.
    SRcd (Map Var (Maybe (Syntax' ty)))
  | -- | Record projection @e.x@
    SProj (Syntax' ty) Var
  | -- | Annotate a term with a type
    SAnnotate (Syntax' ty) Polytype
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable
    , Data
    , Generic
    , FromJSON
    , ToJSON
    , -- | The Traversable instance for Term (and for Syntax') is used during
      -- typechecking: during intermediate type inference, many of the type
      -- annotations placed on AST nodes will have unification variables in
      -- them. Once we have finished solving everything we need to do a
      -- final traversal over all the types in the AST to substitute away
      -- all the unification variables (and generalize, i.e. stick 'forall'
      -- on, as appropriate).  See the call to 'mapM' in
      -- Swarm.Language.Typecheck.runInfer.
      Traversable
    )

instance Data ty => Plated (Term' ty) where
  plate = uniplate

------------------------------------------------------------
-- Basic terms
------------------------------------------------------------

-- | Different runtime behaviors for delayed expressions.
data DelayType
  = -- | A simple delay, implemented via a (non-memoized) @VDelay@
    --   holding the delayed expression.
    SimpleDelay
  | -- | A memoized delay, implemented by allocating a mutable cell
    --   with the delayed expression and returning a reference to it.
    --   When the @Maybe Var@ is @Just@, a recursive binding of the
    --   variable with a reference to the delayed expression will be
    --   provided while evaluating the delayed expression itself. Note
    --   that there is no surface syntax for binding a variable within
    --   a recursive delayed expression; the only way we can get
    --   @Just@ here is when we automatically generate a delayed
    --   expression while interpreting a recursive @let@ or @def@.
    MemoizedDelay (Maybe Var)
  deriving (Eq, Show, Data, Generic, FromJSON, ToJSON)
