{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types representing the surface syntax and terms for Swarm programming language.
module Swarm.Language.Syntax.AST (
  SwarmType,
  Syntax (..),
  LetSyntax (..),
  Term (..),
) where

import Control.Lens (Plated (..))
import Data.Aeson.Types hiding (Key)
import Data.Data (Data, Typeable)
import Data.Data.Lens (uniplate)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Language.Phase
import Swarm.Language.Requirements.Type (Requirements)
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Direction
import Swarm.Language.Syntax.Import (Anchor, ImportLoc)
import Swarm.Language.Syntax.Loc
import Swarm.Language.TDVar (TDVar)
import Swarm.Language.Types
import Swarm.Language.Var (LocVar)

------------------------------------------------------------
-- Syntax: annotation on top of Terms with SrcLoc, comments, + type
------------------------------------------------------------

type family SwarmType (phase :: Phase) where
  SwarmType Raw = ()
  SwarmType Resolved = ()
  SwarmType Inferred = UType
  SwarmType Typed = Polytype
  SwarmType Elaborated = Polytype
  SwarmType Instantiated = Polytype

-- | The surface syntax for the language, with location and type annotations.
data Syntax phase = Syntax
  { _sLoc :: SrcLoc
  , _sTerm :: Term phase
  , _sComments :: Comments
  , _sType :: SwarmType phase
  }
  deriving (Generic)

deriving instance (Eq (Anchor (ImportPhaseFor phase)), Eq (SwarmType phase)) => Eq (Syntax phase)
deriving instance (Show (Anchor (ImportPhaseFor phase)), Show (SwarmType phase)) => Show (Syntax phase)
deriving instance (Data (Anchor (ImportPhaseFor phase)), Data (SwarmType phase), Typeable phase, Typeable (ImportPhaseFor phase)) => Data (Syntax phase)
deriving instance (Hashable (Anchor (ImportPhaseFor phase)), Generic (Anchor (ImportPhaseFor phase)), Hashable (SwarmType phase)) => Hashable (Syntax phase)

instance (Data (Anchor (ImportPhaseFor phase)), Data (SwarmType phase), Typeable phase, Typeable (ImportPhaseFor phase)) => Plated (Syntax phase) where
  plate = uniplate

-- | A @let@ expression can be written either as @let x = e1 in e2@ or
--   as @def x = e1 end; e2@. This enumeration simply records which it
--   was so that we can pretty-print appropriately.
data LetSyntax = LSLet | LSDef
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, Data, Hashable, ToJSON, FromJSON)

------------------------------------------------------------
-- Term: basic syntax tree
------------------------------------------------------------

-- | Terms of the Swarm language.
data Term phase
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
  | -- | An antiquoted Haskell variable name of type Syntax.
    TAntiSyn Text
  | -- | A robot reference.  These never show up in surface syntax, but are
    --   here so we can factor pretty-printing for Values through
    --   pretty-printing for Terms.
    TRobot Int
  | -- | A memory reference.  These likewise never show up in surface syntax,
    --   but are here to facilitate pretty-printing.
    TRef Int
  | -- | Require a specific device to be installed.
    TRequire Text
  | -- | Require a certain number of an entity.
    TStock Int Text
  | -- | Primitive command to log requirements of a term.  The Text
    --   field is to store the unaltered original text of the term, for use
    --   in displaying the log message (since once we get to execution time the
    --   original term may have been elaborated, e.g. `force` may have been added
    --   around some variables, etc.)
    SRequirements Text (Syntax phase)
  | -- | A variable.
    TVar Var
  | -- | A pair.
    SPair (Syntax phase) (Syntax phase)
  | -- | A lambda expression, with or without a type annotation on the
    --   binder.
    SLam LocVar (Maybe Type) (Syntax phase)
  | -- | Function application.
    SApp (Syntax phase) (Syntax phase)
  | -- | A (recursive) let/def expression, with or without a type
    --   annotation on the variable. The @Bool@ indicates whether
    --   it is known to be recursive.
    --
    --   The @Maybe Polytype@ and @Maybe Requirements@ fields are only
    --   for annotating the requirements of a definition after
    --   typechecking; there is no way to annotate requirements in the
    --   surface syntax.
    SLet LetSyntax Bool LocVar (Maybe RawPolytype) (Maybe Polytype) (Maybe Requirements) (Syntax phase) (Syntax phase)
  | -- | A type synonym definition.  Note that this acts like a @let@
    --   (just like @def@), /i.e./ the @Syntax phase@ field is the local
    --   context over which the type definition is in scope.
    STydef (Located TDVar) Polytype (Maybe TydefInfo) (Syntax phase)
  | -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    --
    --   The @Maybe (SwarmType phase)@ field is a place to stash the
    --   inferred type of the variable (if any) during type inference.
    --   Once type inference is complete, during elaboration we will
    --   copy the inferred type into the @Maybe Polytype@ field (since
    --   the @Maybe (SwarmType phase)@ field will be erased).
    --
    --   The @Maybe Polytype@ and @Maybe Requirements@ fields are only
    --   for annotating the type of a bind after typechecking; there
    --   is no surface syntax that allows directly annotating a bind
    --   with either one.
    SBind (Maybe LocVar) (Maybe (SwarmType phase)) (Maybe Polytype) (Maybe Requirements) (Syntax phase) (Syntax phase)
  | -- | Delay evaluation of a term, written @{...}@.  Swarm is an
    --   eager language, but in some cases (e.g. for @if@ statements
    --   and recursive bindings) we need to delay evaluation.  The
    --   counterpart to @{...}@ is @force@, where @force {t} = t@.
    --   Note that 'Force' is just a constant, whereas 'SDelay' has to
    --   be a special syntactic form so its argument can get special
    --   treatment during evaluation.
    SDelay (Syntax phase)
  | -- | Record literals @[x1 = e1, x2 = e2, x3, ...]@ Names @x@
    --   without an accompanying definition are sugar for writing
    --   @x=x@.
    SRcd [(LocVar, Maybe (Syntax phase))]
  | -- | Record projection @e.x@
    SProj (Syntax phase) Var
  | -- | Annotate a term with a type
    SAnnotate (Syntax phase) RawPolytype
  | -- | Run the given command, then suspend and wait for a new REPL
    --   input.
    SSuspend (Syntax phase)
  | -- | An explicit representation of parentheses in the input.  We
    --   need this to be able to print formatted code with parentheses
    --   and comments preserved, but we get rid of them during
    --   elaboration.
    SParens (Syntax phase)
  | -- | A type literal.
    TType Type
  | -- | Import a term containing definitions, which will be in scope
    --   in the following term.
    SImportIn (ImportLoc (ImportPhaseFor phase)) (Syntax phase)
  deriving (Generic)

deriving instance (Eq (Anchor (ImportPhaseFor phase)), Eq (SwarmType phase)) => Eq (Term phase)
deriving instance (Show (Anchor (ImportPhaseFor phase)), Show (SwarmType phase)) => Show (Term phase)
deriving instance (Data (Anchor (ImportPhaseFor phase)), Data (SwarmType phase), Typeable phase, Typeable (ImportPhaseFor phase)) => Data (Term phase)
deriving instance (Generic (Anchor (ImportPhaseFor phase)), Hashable (Anchor (ImportPhaseFor phase)), Hashable (SwarmType phase)) => Hashable (Term phase)

instance (Data (Anchor (ImportPhaseFor phase)), Data (SwarmType phase), Typeable phase, Typeable (ImportPhaseFor phase)) => Plated (Term phase) where
  plate = uniplate
