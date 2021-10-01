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
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Swarm.Language.Syntax
  ( -- * Directions

    Direction(..), applyTurn, toDirection, fromDirection, north, south, east, west

    -- * Constants
  , Const(..), allConst

  , ConstInfo(..), ConstMeta(..), MBinAssoc(..), MUnAssoc(..), constInfo, arity, isCmd, isUserFunc

    -- * Terms
  , Var, Term(..), mkOp

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
import           Witch.From           (from)

import           Swarm.Language.Types

------------------------------------------------------------
-- Constants
------------------------------------------------------------

-- | The type of directions. Used /e.g./ to indicate which way a robot
--   will turn.
data Direction = Lft | Rgt | Back | Fwd | North | South | East | West | Down
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
applyTurn Down _        = V2 0 0

-- | Possibly convert a vector into a 'Direction'---that is, if the
--   vector happens to be a unit vector in one of the cardinal
--   directions.
toDirection :: V2 Int64 -> Maybe Direction
toDirection v    = case v of
  V2 0 1    -> Just North
  V2 0 (-1) -> Just South
  V2 1 0    -> Just East
  V2 (-1) 0 -> Just West
  V2 0 0    -> Just Down
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
  = Noop              -- ^ Do nothing.  This is different than 'Wait'
                      --   in that it does not take up a time step.
  | Wait              -- ^ Wait for one time step without doing anything.
  | Selfdestruct      -- ^ Self-destruct.

  -- Basic actions
  | Move              -- ^ Move forward one step.
  | Turn              -- ^ Turn in some direction.
  | Grab              -- ^ Grab an item from the current location.
  | Place             -- ^ Try to place an item at the current location.
  | Give              -- ^ Give an item to another robot at the current location.
  | Install           -- ^ Install a device on a robot.
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
  | Scan              -- ^ Scan a nearby cell
  | Upload            -- ^ Upload knowledge to another robot
  | Ishere            -- ^ See if a specific entity is here. (This may be removed.)
  | Random            -- ^ Get a uniformly random integer.

  -- Modules
  | Run               -- ^ Run a program loaded from a file.

  -- Language built-ins
  | If                -- ^ If-expressions.
  | Fst               -- ^ First projection.
  | Snd               -- ^ Second projection.
  | Force             -- ^ Force a delayed evaluation.
  | Return            -- ^ Return for the cmd monad.
  | Try               -- ^ Try/catch block
  | Raise             -- ^ Raise an exception

  -- Arithmetic unary operators
  | Not               -- ^ Logical negation.
  | Neg               -- ^ Arithmetic negation.

  -- Comparison operators (check for with isCmpBinOp)
  | Eq                -- ^ Logical equality comparison
  | Neq               -- ^ Logical unequality comparison
  | Lt                -- ^ Logical lesser-then comparison
  | Gt                -- ^ Logical greater-then comparison
  | Leq               -- ^ Logical lesser-or-equal comparison
  | Geq               -- ^ Logical greater-or-equal comparison

  -- Arithmetic binary operators (check for with isArithBinOp)
  | Add               -- ^ Arithmetic addition operator
  | Sub               -- ^ Arithmetic subtraction operator
  | Mul               -- ^ Arithmetic multiplication operator
  | Div               -- ^ Arithmetic division operator
  | Exp               -- ^ Arithmetic exponentiation operator

  deriving (Eq, Ord, Enum, Bounded, Data, Show)

allConst :: [Const]
allConst = [minBound..maxBound]

data ConstInfo = ConstInfo
  { syntax :: Text
  , fixity :: Int
  , constMeta :: ConstMeta
  }
  deriving (Eq, Ord, Show)

data ConstMeta
  = ConstMFunc Int Bool     -- ^ Function with arity of which some are commands
  | ConstMUnOp MUnAssoc     -- ^ Unary operator with fixity and associativity.
  | ConstMBinOp MBinAssoc   -- ^ Binary operator with fixity and associativity.
  deriving (Eq, Ord, Show)

-- | The meta type representing associativity of binary operator.
data MBinAssoc
  = L   -- ^  Left associative binary operator (see 'Control.Monad.Combinators.Expr.InfixL')
  | N   -- ^   Non-associative binary operator (see 'Control.Monad.Combinators.Expr.InfixN')
  | R   -- ^ Right associative binary operator (see 'Control.Monad.Combinators.Expr.InfixR')
  deriving (Eq, Ord, Show)

-- | The meta type representing associativity of unary operator.
data MUnAssoc
  = P   -- ^  Prefix unary operator (see 'Control.Monad.Combinators.Expr.Prefix')
  | S   -- ^  Suffix unary operator (see 'Control.Monad.Combinators.Expr.Suffix')
  deriving (Eq, Ord, Show)

-- | The arity of a constant, /i.e./ how many arguments it expects.
--   The runtime system will collect arguments to a constant (see
--   'Swarm.Game.Value.VCApp') until it has enough, then dispatch
--   the constant's behavior.
arity :: Const -> Int
arity c = case constMeta $ constInfo c of
  ConstMUnOp {} -> 1
  ConstMBinOp {} -> 2
  ConstMFunc a _ -> a

-- note: verified same as before
isCmd :: Const -> Bool
isCmd c = case constMeta $ constInfo c of
  ConstMFunc _ cmd -> cmd
  _ -> False

-- | Function constants user can call with reserved words ('wait',...).
isUserFunc :: Const -> Bool
isUserFunc c = case constMeta $ constInfo c of
  ConstMFunc {} -> c `notElem` [Noop, Force]
  _ -> False

-- | Information about constants used in parsing and pretty printing.
--
-- It would be more compact to represent the information by testing
-- whether the constants are in certain sets, but using pattern
-- matching gives us warning if we add more constants.
constInfo :: Const -> ConstInfo
constInfo c = case c of
  Wait         -> commandLow 0
  Noop         -> command "{}" 0
  Selfdestruct -> commandLow 0
  Move         -> commandLow 0
  Turn         -> commandLow 1
  Grab         -> commandLow 0
  Place        -> commandLow 1
  Give         -> commandLow 2
  Install      -> commandLow 2
  Make         -> commandLow 1
  Build        -> commandLow 2
  Say          -> commandLow 1
  View         -> commandLow 1
  Appear       -> commandLow 1
  Create       -> commandLow 1
  GetX         -> commandLow 0
  GetY         -> commandLow 0
  Blocked      -> commandLow 0
  Scan         -> commandLow 0
  Upload       -> commandLow 1
  Ishere       -> commandLow 1
  Random       -> commandLow 1
  Run          -> commandLow 1
  Return       -> commandLow 1
  Try          -> commandLow 2
  Raise        -> commandLow 1
  If           -> functionLow 3
  Fst          -> functionLow 1
  Snd          -> functionLow 1
  Force        -> functionLow 1 -- TODO: make internal?!
  Not          -> functionLow 1
  Neg          -> unaryOp  "-"  7 P
  Add          -> binaryOp "+"  6 L
  Sub          -> binaryOp "-"  6 L
  Mul          -> binaryOp "*"  7 L
  Div          -> binaryOp "/"  7 L
  Exp          -> binaryOp "^"  8 R
  Eq           -> binaryOp "==" 4 N
  Neq          -> binaryOp "/=" 4 N
  Lt           -> binaryOp ">=" 4 N
  Gt           -> binaryOp "<=" 4 N
  Leq          -> binaryOp ">"  4 N
  Geq          -> binaryOp "<"  4 N
  where
    unaryOp  s p side = ConstInfo {syntax=s, fixity=p, constMeta=ConstMUnOp side}
    binaryOp s p side = ConstInfo {syntax=s, fixity=p, constMeta=ConstMBinOp side}
    command  s a      = ConstInfo {syntax=s, fixity=11, constMeta=ConstMFunc a True}
    function s a      = ConstInfo {syntax=s, fixity=11, constMeta=ConstMFunc a False}
    commandLow  = command  (lowShow c)
    functionLow = function (lowShow c)

-- | Make infix operation (e.g. @2 + 3@) a curried function
--   application (@((+) 2) 3@).
mkOp :: Const -> Term -> Term -> Term
mkOp c = TApp . TApp (TConst c)

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

    -- | An antiquoted Haskell variable name of type Integer.
  | TAntiInt Text

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
      TAntiInt{} -> pure t
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

lowShow :: Show a => a -> Text
lowShow a = toLower (from (show a))
