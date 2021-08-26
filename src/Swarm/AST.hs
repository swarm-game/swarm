{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Swarm.AST where

import qualified Data.Functor.Const    as C
import           Data.Functor.Identity (Identity)
import           Data.Text

import           Swarm.Types

type Var = Text

data Direction
  = Lt
  | Rt
  | Back
  | Fwd
  | North
  | South
  | East
  | West
  deriving (Eq, Ord, Show, Read)

-- | The Term' type is parameterized by a functor that expresses how
--   much type information we have.
--
--   - When f = C.Const (), we have no type information at all.
--   - When f = Maybe, we might have some type information (e.g. type
--     annotations supplied in the surface syntax)
--   - When f = Identity, we have all type information.
--
--   The type annotations are placed strategically to maintain the
--   following invariant: given the type of a term as input, we can
--   reconstruct the types of all subterms.

data Term' f
  = TUnit
  | TConst Const
  | TDir Direction
  | TInt Integer
  | TString Text
  | TBool Bool
  | TVar (f Type) Var
  | TLam Var (f Type) (Term' f)
  | TApp (f Type) (Term' f) (Term' f)
  | TLet Var (f Type) (Term' f) (Term' f)
  | TBind (Maybe Var) (f Type) (Term' f) (Term' f)
  | TNop
  | TDelay (Term' f)
    -- TDelay has to be a special form --- not just an application of
    -- a constant --- so it can get special treatment during
    -- evaluation.

deriving instance Eq (f Type) => Eq (Term' f)
deriving instance Ord (f Type) => Ord (Term' f)
deriving instance Show (f Type) => Show (Term' f)

type Term = Term' Maybe
type ATerm = Term' Identity
type UTerm = Term' (C.Const ())

mapTerm' :: (f Type -> g Type) -> Term' f -> Term' g
mapTerm' _ TUnit              = TUnit
mapTerm' _ (TConst co)        = TConst co
mapTerm' _ (TDir di)          = TDir di
mapTerm' _ (TInt n)           = TInt n
mapTerm' _ (TString s)        = TString s
mapTerm' _ (TBool b)          = TBool b
mapTerm' h (TVar ty x)        = TVar (h ty) x
mapTerm' h (TLam x ty t)      = TLam x (h ty) (mapTerm' h t)
mapTerm' h (TApp ty2 t1 t2)   = TApp (h ty2) (mapTerm' h t1) (mapTerm' h t2)
mapTerm' h (TLet x ty t1 t2)  = TLet x (h ty) (mapTerm' h t1) (mapTerm' h t2)
mapTerm' h (TBind x ty t1 t2) = TBind x (h ty) (mapTerm' h t1) (mapTerm' h t2)
mapTerm' _ TNop               = TNop
mapTerm' h (TDelay t)         = TDelay (mapTerm' h t)

erase :: Term' f -> UTerm
erase = mapTerm' (const (C.Const ()))

-- | Built-in function and command constants.
data Const
  = Wait
  | Move
  | Turn
  | Harvest
  | Repeat    -- XXX get rid of repeat, encode it as a function within the language
  | Build
  | Run
  | GetX
  | GetY
  | If
  | Force
  deriving (Eq, Ord, Show)

-- | The arity of a constant.
arity :: Const -> Int
arity Wait    = 0
arity Move    = 0
arity Turn    = 1
arity Harvest = 0
arity Repeat  = 2
arity Build   = 1
arity Run     = 1
arity GetX    = 0
arity GetY    = 0
arity If      = 3
arity Force   = 1

-- | Some constants are commands, which means a fully saturated
--   application of those constants counts as a value, and should not
--   be reduced further until they are to be executed (i.e. until they
--   meet an FExec frame).  Other constants just represent pure
--   functions; fully saturated applications of such constants should
--   be evaluated immediately.
isCmd :: Const -> Bool
isCmd c = c `notElem` funList
  where
    funList = [If, Force]
