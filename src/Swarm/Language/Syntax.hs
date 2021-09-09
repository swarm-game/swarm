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
  , Var, Term(..)

    -- * Term traversal

  , bottomUp, fv, mapFree

  ) where

import           Data.Set             (Set)
import qualified Data.Set             as S
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
  | Grab              -- ^ Grab an item from the current location.
  | Place             -- ^ Try to place an item at the current location.
  | Give              -- ^ Give an item to another robot at the current location.
  | Craft             -- ^ Craft an item.
  | Build             -- ^ Construct a new robot.
  | Run               -- ^ Run a program loaded from a file.
  | GetX              -- ^ Get the current x-coordinate.
  | GetY              -- ^ Get the current y-coordinate.
  | Random            -- ^ Get a uniformly random integer.
  | Say               -- ^ Emit a message.
  | View              -- ^ View a certain robot.
  | Appear            -- ^ Set what characters are used for display.
  | IsHere  -- XXX for testing, see if a specific entity is here
            -- probably going to remove this later
  | If                -- ^ If-expressions.
  | Fst               -- ^ First projection.
  | Snd               -- ^ Second projection.
  | Force             -- ^ Force a delayed evaluation.
  | Not               -- ^ Logical negation.
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
  | c `elem` [ Wait, Noop, Halt, Move, Grab, Place, Give
             , Craft, GetX, GetY]                           = 0
  | c `elem` [ Return, Turn, Run, Random, Say, View, Appear, IsHere
             , Not, Fst, Snd, Force ]                       = 1
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
    funList = [If, Force, Not, Fst, Snd]

------------------------------------------------------------
-- Terms

-- | Terms of the Swarm language.

data Term
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
  | TPair Term Term

    -- | A lambda expression, with or without a type annotation on the
    --   binder.
  | TLam Var (Maybe Type) Term

    -- | Application, possibly with a type annotation telling us the
    --   type of the argument, which would otherwise be impossible to
    --   figure out from the overall result type.
  | TApp Term Term

    -- | A __recursive__ let expression, with or without a type
    --   annotation on the variable.
  | TLet Var (Maybe Polytype) Term Term

    -- | A (recursive) definition command, which binds a variable to a
    --   value in subsequent commands.
  | TDef Var (Maybe Polytype) Term

    -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    --   The type annotation tells us the type of @c1@.
  | TBind (Maybe Var) Term Term

    -- | Delay evaluation of a term.  Swarm is an eager language, but
    --   in some cases (e.g. for @if@ statements and recursive
    --   bindings) we need to delay evaluation.  The counterpart to
    --   @delay@ is @force@, where @force (delay t) = t@.  Note that
    --   @force@ is just a constant, whereas 'TDelay' has to be a
    --   special syntactic form so its argument can get special
    --   treatment during evaluation.
  | TDelay Term
  deriving (Eq, Show)

-- | Rewrite a term using a bottom-up traversal.
bottomUp :: (Term -> Term) -> Term -> Term
bottomUp f (TPair t1 t2) = f (TPair (bottomUp f t1) (bottomUp f t2))
bottomUp f (TLam x xTy t) = f (TLam x xTy (bottomUp f t))
bottomUp f (TApp t1 t2)
  = f (TApp (bottomUp f t1) (bottomUp f t2))
bottomUp f (TDef x xTy t)
  = f (TDef x xTy (bottomUp f t))
bottomUp f (TLet x xTy t1 t2)
  = f (TLet x xTy (bottomUp f t1) (bottomUp f t2))
bottomUp f (TBind mx t1 t2)
  = f (TBind mx (bottomUp f t1) (bottomUp f t2))
bottomUp f (TDelay t) = f (TDelay (bottomUp f t))
bottomUp f t = f t

-- | The free variables of a term.
fv :: Term -> Set Var
fv (TVar x)               = S.singleton x
fv (TPair t1 t2)          = fv t1 `S.union` fv t2
fv (TLam x _ t)           = S.delete x (fv t)
fv (TApp t1 t2)           = fv t1 `S.union` fv t2
fv (TLet x _ t1 t2)       = S.delete x (fv t1 `S.union` fv t2)
fv (TDef x _ t)           = S.delete x (fv t)
fv (TBind (Just x) t1 t2) = fv t1 `S.union` S.delete x (fv t2)
fv (TBind Nothing  t1 t2) = fv t1 `S.union` fv t2
fv (TDelay t)             = fv t
fv _                      = S.empty

-- | Apply a function to all the free occurrences of a variable.
mapFree :: Var -> (Term -> Term) -> Term -> Term
mapFree x f (TVar y)
  | x == y    = f (TVar y)
  | otherwise = TVar y
mapFree x f (TPair t1 t2) = TPair (mapFree x f t1) (mapFree x f t2)
mapFree x f t@(TLam y ty body)
  | x == y = t
  | otherwise = TLam y ty (mapFree x f body)
mapFree x f (TApp t1 t2) = TApp (mapFree x f t1) (mapFree x f t2)
mapFree x f t@(TLet y ty t1 t2)
  | x == y = t
  | otherwise = TLet y ty (mapFree x f t1) (mapFree x f t2)
mapFree x f t@(TDef y ty t1)
  | x == y = t
  | otherwise = TDef y ty (mapFree x f t1)
mapFree x f (TBind mx t1 t2)
  | Just y <- mx, x == y = TBind mx (mapFree x f t1) t2
  | otherwise = TBind mx (mapFree x f t1) (mapFree x f t2)
mapFree x f (TDelay t) = TDelay (mapFree x f t)
mapFree _ _ t = t
