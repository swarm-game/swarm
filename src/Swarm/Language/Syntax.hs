-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Syntax
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for terms of the Swarm programming language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Swarm.Language.Syntax
  ( -- * Directions

    Direction(..), applyTurn, toDirection, fromDirection, north, south, east, west

    -- * Constants
  , Const(..), CmpConst(..), ArithConst(..)

  , arity, isCmd

    -- * Terms
  , Var, Term(..)

    -- * Term traversal

  , fvT, fv, mapFree1

  ) where

import           Control.Lens         (Plated (..), Traversal', (%~))
import           Data.Data.Lens       (uniplate)
import           Data.Int             (Int64)
import qualified Data.Set             as S
import           Data.Text
import           Linear

import           Data.Aeson.Types
import           Data.Data            (Data)
import           Data.Hashable        (Hashable)
import           GHC.Generics         (Generic)

import           Swarm.Language.Types

------------------------------------------------------------
-- Constants
------------------------------------------------------------

-- | The type of directions. Used /e.g./ to indicate which way a robot
--   will turn.
data Direction = Lft | Rgt | Back | Fwd | North | South | East | West
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, ToJSON, FromJSON)

instance ToJSONKey Direction where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Direction where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

-- | The 'applyTurn' function gives the meaning of each 'Direction' by
--   turning relative to the given vector or by turning to an absolute
--   direction vector.
applyTurn :: Direction -> V2 Int64 -> V2 Int64
applyTurn Lft (V2 x y)  = V2 (-y) x
applyTurn Rgt (V2 x y)  = V2 y (-x)
applyTurn Back (V2 x y) = V2 (-x) (-y)
applyTurn Fwd v         = v
applyTurn North _       = north
applyTurn South _       = south
applyTurn East _        = east
applyTurn West _        = west

-- | Possibly convert a vector into a 'Direction'---that is, if the
--   vector happens to be a unit vector in one of the cardinal
--   directions.
toDirection :: V2 Int64 -> Maybe Direction
toDirection v    = case v of
  V2 0 1    -> Just North
  V2 0 (-1) -> Just South
  V2 1 0    -> Just East
  V2 (-1) 0 -> Just West
  _         -> Nothing

-- | Convert a 'Direction' into a corresponding vector.  Note that
--   this only does something reasonable for 'North', 'South', 'East',
--   and 'West'---other 'Direction's return the zero vector.
fromDirection :: Direction -> V2 Int64
fromDirection d = case d of
  North -> north
  South -> south
  East  -> east
  West  -> west
  _     -> V2 0 0

-- | The cardinal direction north = @V2 0 1@.
north :: V2 Int64
north = V2 0 1

-- | The cardinal direction south = @V2 0 (-1)@.
south :: V2 Int64
south = V2 0 (-1)

-- | The cardinal direction east = @V2 1 0@.
east :: V2 Int64
east  = V2 1 0

-- | The cardinal direction west = @V2 (-1) 0@.
west :: V2 Int64
west  = V2 (-1) 0

-- | Constants, representing various built-in functions and commands.
data Const

  -- Trivial actions
  = Wait              -- ^ Wait for one time step without doing anything.
  | Noop              -- ^ Do nothing.  This is different than 'Wait'
                      --   in that it does not take up a time step.
  | Selfdestruct      -- ^ Self-destruct.

  -- Basic actions
  | Move              -- ^ Move forward one step.
  | Turn              -- ^ Turn in some direction.
  | Grab              -- ^ Grab an item from the current location.
  | Place             -- ^ Try to place an item at the current location.
  | Give              -- ^ Give an item to another robot at the current location.
  | Make              -- ^ Make an item.
  | Build             -- ^ Construct a new robot.
  | Say               -- ^ Emit a message.
  | View              -- ^ View a certain robot.
  | Appear            -- ^ Set what characters are used for display.
  | Create            -- ^ Create an entity out of thin air. Only
                      --   available in creative mode.

  -- Sensing / generation
  | GetX              -- ^ Get the current x-coordinate.
  | GetY              -- ^ Get the current y-coordinate.
  | Blocked           -- ^ See if we can move forward or not.
  | Ishere            -- ^ See if a specific entity is here. (This may be removed.)
  | Random            -- ^ Get a uniformly random integer.

  -- Modules
  | Run               -- ^ Run a program loaded from a file.

  -- Arithmetic
  | Not               -- ^ Logical negation.
  | Cmp CmpConst      -- ^ Binary comparison operators.
  | Neg               -- ^ Arithmetic negation.
  | Arith ArithConst  -- ^ Binary arithmetic operators.

  -- Language built-ins
  | If                -- ^ If-expressions.
  | Fst               -- ^ First projection.
  | Snd               -- ^ Second projection.
  | Force             -- ^ Force a delayed evaluation.
  | Return            -- ^ Return for the cmd monad.
  | Try               -- ^ Try/catch block
  | Raise             -- ^ Raise an exception

  deriving (Eq, Ord, Show, Data)

-- | Comparison operator constants.
data CmpConst = CmpEq | CmpNeq | CmpLt | CmpGt | CmpLeq | CmpGeq
  deriving (Eq, Ord, Show, Data)

-- | Arithmetic operator constants.
data ArithConst = Add | Sub | Mul | Div | Exp
  deriving (Eq, Ord, Show, Data)

-- | The arity of a constant, /i.e./ how many arguments it expects.
--   The runtime system will collect arguments to a constant (see
--   'Swarm.Game.Value.VCApp') until it has enough, then dispatch the constant's
--   behavior.
arity :: Const -> Int
arity Wait         = 0
arity Noop         = 0
arity Selfdestruct = 0
arity Move         = 0
arity Turn         = 1
arity Grab         = 0
arity Place        = 1
arity Give         = 2
arity Make         = 1
arity Build        = 2
arity Say          = 1
arity View         = 1
arity Appear       = 1
arity Create       = 1
arity GetX         = 0
arity GetY         = 0
arity Blocked      = 0
arity Ishere       = 1
arity Random       = 1
arity Run          = 1
arity Not          = 1
arity (Cmp _)      = 2
arity Neg          = 1
arity (Arith _)    = 2
arity If           = 3
arity Fst          = 1
arity Snd          = 1
arity Force        = 1
arity Return       = 1
arity Try          = 2
arity Raise        = 1
  -- It would be more compact to represent the above by testing
  -- whether the constants are in certain sets, but this way the
  -- compiler warns us about incomplete pattern match if we add more
  -- constants.

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
    funList = [If, Force, Not, Neg, Fst, Snd]

------------------------------------------------------------
-- Terms

-- | Terms of the Swarm language.

data Term
    -- | The unit value.
  = TUnit

    -- | A constant.
  | TConst Const

    -- | A direction literal.
  | TDir Direction

    -- | An integer literal.
  | TInt Integer

    -- | A string literal.
  | TString Text

    -- | An antiquoted Haskell variable name of type Text.
  | TAntiString Text

    -- | A Boolean literal.
  | TBool Bool

    -- | A variable.
  | TVar Var

    -- | A pair.
  | TPair Term Term

    -- | A lambda expression, with or without a type annotation on the
    --   binder.
  | TLam Var (Maybe Type) Term

    -- | Function application.
  | TApp Term Term

    -- | A (recursive) let expression, with or without a type
    --   annotation on the variable.
  | TLet Var (Maybe Polytype) Term Term

    -- | A (recursive) definition command, which binds a variable to a
    --   value in subsequent commands.
  | TDef Var (Maybe Polytype) Term

    -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
  | TBind (Maybe Var) Term Term

    -- | Delay evaluation of a term.  Swarm is an eager language, but
    --   in some cases (e.g. for @if@ statements and recursive
    --   bindings) we need to delay evaluation.  The counterpart to
    --   @delay@ is @force@, where @force (delay t) = t@.  Note that
    --   'Force' is just a constant, whereas 'TDelay' has to be a
    --   special syntactic form so its argument can get special
    --   treatment during evaluation.
  | TDelay Term
  deriving (Eq, Show, Data)

instance Plated Term where
  plate = uniplate

-- | Traversal over those subterms of a term which represent free
--   variables.
fvT :: Traversal' Term Term
fvT f = go S.empty
  where
    go bound t = case t of
      TUnit -> pure t
      TConst{} -> pure t
      TDir{} -> pure t
      TInt{} -> pure t
      TString{} -> pure t
      TAntiString{} -> pure t
      TBool{} -> pure t
      TVar x
        | x `S.member` bound -> pure t
        | otherwise          -> f (TVar x)
      TLam x ty t1 -> TLam x ty <$> go (S.insert x bound) t1
      TApp t1 t2  -> TApp <$> go bound t1 <*> go bound t2
      TLet x ty t1 t2 ->
        let bound' = S.insert x bound
        in  TLet x ty <$> go bound' t1 <*> go bound' t2
      TPair t1 t2 -> TPair <$> go bound t1 <*> go bound t2
      TDef x ty t1 -> TDef x ty <$> go (S.insert x bound) t1
      TBind mx t1 t2 ->
        TBind mx <$> go bound t1 <*> go (maybe id S.insert mx bound) t2
      TDelay t1 -> TDelay <$> go bound t1

-- | Traversal over the free variables of a term.  Note that if you
--   want to get the set of all free variables, you can do so via
--   @'Data.Set.Lens.setOf' 'fv'@.
fv :: Traversal' Term Var
fv = fvT . (\f -> \case { TVar x -> TVar <$> f x ; t -> pure t })

-- | Apply a function to all free occurrences of a particular variable.
mapFree1 :: Var -> (Term -> Term) -> Term -> Term
mapFree1 x f = fvT %~ (\t -> if t == TVar x then f t else t)
