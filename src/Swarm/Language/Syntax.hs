-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Syntax
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for the Swarm programming language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Swarm.Language.Syntax
  ( -- * Constants

    Direction(..), applyTurn, north, south, east, west
  , Const(..), CmpConst(..), ArithConst(..)

  , arity, isCmd

    -- * Terms
  , Var, Term'(..), UTerm, Term, ATerm, pattern ID, pattern NONE

  , mapTerm', erase

    -- * Term traversal

  , bottomUp, fv, mapFree

  ) where

import qualified Data.Functor.Const    as C
import           Data.Functor.Identity
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Text
import           Linear

import           Swarm.Language.Types

------------------------------------------------------------
-- Constants
------------------------------------------------------------

-- | The type of directions. Used /e.g./ to indicate which way a robot
--   will turn.
data Direction = Lft | Rgt | Back | Fwd | North | South | East | West
  deriving (Eq, Ord, Show, Read)

-- | The 'applyTurn' function gives the meaning of each 'Direction' by
--   turning relative to the given vector or by turning to an absolute
--   direction vector.
applyTurn :: Direction -> V2 Int -> V2 Int
applyTurn Lft (V2 x y)  = V2 (-y) x
applyTurn Rgt (V2 x y)  = V2 y (-x)
applyTurn Back (V2 x y) = V2 (-x) (-y)
applyTurn Fwd v         = v
applyTurn North _       = north
applyTurn South _       = south
applyTurn East _        = east
applyTurn West _        = west

-- | The cardinal directions.
north, south, east, west :: V2 Int
north = V2 0 1
south = V2 0 (-1)
east  = V2 1 0
west  = V2 (-1) 0

-- | Constants, representing various built-in functions and commands.
data Const
  = Wait              -- ^ Wait for one time step without doing anything.
  | Noop              -- ^ Do nothing.  This is different than 'Wait'
                      --   in that it does not take up a time step.
  | Halt              -- ^ Self-destruct.
  | Return            -- ^ Return for the cmd monad.
  | Move              -- ^ Move forward one step.
  | Turn              -- ^ Turn in some direction.
  | Harvest           -- ^ Pick up an item from the current location.
  | Build             -- ^ Construct a new robot.
  | Run               -- ^ Run a program loaded from a file.
  | GetX              -- ^ Get the current x-coordinate.
  | GetY              -- ^ Get the current y-coordinate.
  | Random            -- ^ Get a uniformly random integer.
  | Say               -- ^ Emit a message.
  | View              -- ^ View a certain robot.
  | Appear            -- ^ Set what characters are used for display.
  | If                -- ^ If-expressions.
  | Fst               -- ^ First projection.
  | Snd               -- ^ Second projection.
  | Force             -- ^ Force a delayed evaluation.
  | Cmp CmpConst      -- ^ Comparison operators.
  | Arith ArithConst  -- ^ Arithmetic operators.
  deriving (Eq, Ord, Show)

-- | Comparison operator constants.
data CmpConst = CmpEq | CmpNeq | CmpLt | CmpGt | CmpLeq | CmpGeq
  deriving (Eq, Ord, Show)

-- | Arithmetic operator constants.
data ArithConst = Neg | Add | Sub | Mul | Div | Exp
  deriving (Eq, Ord, Show)

-- | The arity of a constant, /i.e./ how many arguments it expects.
--   The runtime system will collect arguments to a constant (see
--   'Swarm.Game.Value.VCApp') until it has enough, then dispatch the constant's
--   behavior.
arity :: Const -> Int
arity (Cmp _)     = 2
arity (Arith Neg) = 1
arity (Arith _)   = 2
arity c
  | c `elem` [ Wait, Noop, Halt, Move, Harvest, GetX, GetY] = 0
  | c `elem` [ Return, Turn, Run, Random, Say, View, Appear
             , Fst, Snd, Force ]                            = 1
  | c == Build                                              = 2
  | otherwise                                               = 3

-- | Some constants are commands, which means a fully saturated
--   application of those constants counts as a value, and should not
--   be reduced further until it is to be executed (i.e. until it
--   meets an 'Swarm.Game.CEK.FExec' frame).  Other constants just represent pure
--   functions; fully saturated applications of such constants should
--   be evaluated immediately.
isCmd :: Const -> Bool
isCmd (Cmp _)   = False
isCmd (Arith _) = False
isCmd c = c `notElem` funList
  where
    funList = [If, Force, Fst, Snd]

------------------------------------------------------------
-- Terms

-- | We use 'Text' values to represent variables.
type Var = Text

-- | The 'Term'' type is parameterized by a functor that expresses how
--   much type information we have, for a very lightweight way of
--   having different levels of annotation at different phases.
--
--   - When @f = C.Const ()@, we have no type information at all.
--     This corresponds to the 'UTerm' (Untyped Term) type synonym.
--   - When @f = Maybe@, we might have some type information (e.g. type
--     annotations supplied in the surface syntax). This corresponds to 'Term'.
--   - When @f = Identity@, we have all type information. This
--     coresponds to 'ATerm' (Annotated Term).
--
--   Generally, we start out with a 'Term' from the parser; the
--   typechecker then annotates it into an 'ATerm'; before handing a
--   term off to the interpreter, the type annotations are erased,
--   turning it into a 'UTerm'.
--
--   The type annotations in an 'ATerm' are placed strategically to
--   maintain the following invariant: given the type of a term as
--   input, we can reconstruct the types of all subterms.
--   Additionally, some annotations which would not otherwise be
--   needed to maintain the invariant (/e.g./ the type annotation on the
--   binder of a lambda) are there to allow the user to give hints to
--   help type inference.

data Term' f
    -- | The unit value.
  = TUnit

    -- | A constant.
  | TConst Const

    -- | A direction.
  | TDir Direction

    -- | An integer literal.
  | TInt Integer

    -- | A string literal.
  | TString Text

    -- | A Boolean literal.
  | TBool Bool

    -- | A variable.
  | TVar Var

    -- | A pair.
  | TPair (Term' f) (Term' f)

    -- | A lambda expression, with or without a type annotation on the
    --   binder.
  | TLam Var (f Type) (Term' f)

    -- | Application, possibly with a type annotation telling us the
    --   type of the argument, which would otherwise be impossible to
    --   figure out from the overall result type.
  | TApp (f Type) (Term' f) (Term' f)

    -- | A __recursive__ let expression, with or without a type
    --   annotation on the variable.
  | TLet Var (f Type) (Term' f) (Term' f)

    -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    --   The type annotation tells us the /result/ type of @c1@.
  | TBind (Maybe Var) (f Type) (Term' f) (Term' f)

    -- | Delay evaluation of a term.  Swarm is an eager language, but
    --   in some cases (e.g. for @if@ statements and recursive
    --   bindings) we need to delay evaluation.  The counterpart to
    --   @delay@ is @force@, where @force (delay t) = t@.  Note that
    --   @force@ is just a constant, whereas 'TDelay' has to be a
    --   special syntactic form so its argument can get special
    --   treatment during evaluation.
  | TDelay (Term' f)

deriving instance Eq (f Type) => Eq (Term' f)
deriving instance Ord (f Type) => Ord (Term' f)
deriving instance Show (f Type) => Show (Term' f)

-- | Terms with some type annotations.
type Term = Term' Maybe

-- | Terms with all type annotations.
type ATerm = Term' Identity

-- | Terms with no type annotations.
type UTerm = Term' (C.Const ())

-- | Convenient synonym for the 'Identity' constructor, to be used to
--   build and pattern match on @'ATerm' = Term' Identity@ values.
pattern ID :: a -> Identity a
pattern ID a = Identity a

-- | Convenient synonym for @Const ()@, to be used to build and
--   pattern match on @'UTerm' = Term' (Const ())@ values.
pattern NONE :: C.Const () a
pattern NONE = C.Const ()

-- | Change one sort of term into another, by updating the type information.
mapTerm' :: (f Type -> g Type) -> Term' f -> Term' g
mapTerm' _ TUnit              = TUnit
mapTerm' _ (TConst co)        = TConst co
mapTerm' _ (TDir di)          = TDir di
mapTerm' _ (TInt n)           = TInt n
mapTerm' _ (TString s)        = TString s
mapTerm' _ (TBool b)          = TBool b
mapTerm' _ (TVar x)           = TVar x
mapTerm' h (TPair t1 t2)      = TPair (mapTerm' h t1) (mapTerm' h t2)
mapTerm' h (TLam x ty t)      = TLam x (h ty) (mapTerm' h t)
mapTerm' h (TApp ty2 t1 t2)   = TApp (h ty2) (mapTerm' h t1) (mapTerm' h t2)
mapTerm' h (TLet x ty t1 t2)  = TLet x (h ty) (mapTerm' h t1) (mapTerm' h t2)
mapTerm' h (TBind x ty t1 t2) = TBind x (h ty) (mapTerm' h t1) (mapTerm' h t2)
mapTerm' h (TDelay t)         = TDelay (mapTerm' h t)

-- | Erase the type annotations in a term.
erase :: Term' f -> UTerm
erase = mapTerm' (const (C.Const ()))

-- | Rewrite a term using a bottom-up traversal.  Giving the rewriting
--   function access to the type of each subtree.
bottomUp :: (Type -> ATerm -> ATerm) -> Type -> ATerm -> ATerm
bottomUp f ty@(ty1 :*: ty2) (TPair t1 t2) = f ty (TPair (bottomUp f ty1 t1) (bottomUp f ty2 t2))
bottomUp f ty@(_ :->: ty2) (TLam x xTy t) = f ty (TLam x xTy (bottomUp f ty2 t))
bottomUp f ty (TApp ity2@(ID ty2) t1 t2)
  = f ty (TApp ity2 (bottomUp f (ty2 :->: ty) t1) (bottomUp f ty2 t2))
bottomUp f ty (TLet x xTy@(ID ty1) t1 t2)
  = f ty (TLet x xTy (bottomUp f ty1 t1) (bottomUp f ty t2))
bottomUp f ty2 (TBind mx ia@(ID a) t1 t2)
  = f ty2 (TBind mx ia (bottomUp f (TyCmd a) t1) (bottomUp f ty2 t2))
bottomUp f ty (TDelay t) = f ty (TDelay (bottomUp f ty t))
bottomUp f ty t = f ty t

-- | The free variables of a term.
fv :: Term' f -> Set Var
fv (TVar x)                 = S.singleton x
fv (TPair t1 t2)            = fv t1 `S.union` fv t2
fv (TLam x _ t)             = S.delete x (fv t)
fv (TApp _ t1 t2)           = fv t1 `S.union` fv t2
fv (TLet x _ t1 t2)         = S.delete x (fv t1 `S.union` fv t2)
fv (TBind (Just x) _ t1 t2) = fv t1 `S.union` S.delete x (fv t2)
fv (TBind Nothing  _ t1 t2) = fv t1 `S.union` fv t2
fv (TDelay t)               = fv t
fv _                        = S.empty

-- | Apply a function to all the free occurrences of a variable.
mapFree :: Var -> (ATerm -> ATerm) -> ATerm -> ATerm
mapFree x f (TVar y)
  | x == y    = f (TVar y)
  | otherwise = TVar y
mapFree x f (TPair t1 t2) = TPair (mapFree x f t1) (mapFree x f t2)
mapFree x f t@(TLam y ty body)
  | x == y = t
  | otherwise = TLam y ty (mapFree x f body)
mapFree x f (TApp ty t1 t2) = TApp ty (mapFree x f t1) (mapFree x f t2)
mapFree x f t@(TLet y ty t1 t2)
  | x == y = t
  | otherwise = TLet y ty (mapFree x f t1) (mapFree x f t2)
mapFree x f (TBind mx ty t1 t2)
  | Just y <- mx, x == y = TBind mx ty (mapFree x f t1) t2
  | otherwise = TBind mx ty (mapFree x f t1) (mapFree x f t2)
mapFree x f (TDelay t) = TDelay (mapFree x f t)
mapFree _ _ t = t
