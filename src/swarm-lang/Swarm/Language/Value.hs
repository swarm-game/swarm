{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Values and environments used for interpreting the Swarm language.
module Swarm.Language.Value (
  -- * Values
  Value (..),
  prettyValue,
  valueToTerm,

  -- * Environments
  Env,
  emptyEnv,
  envTypes,
  envReqs,
  envVals,
  envTydefs,
  lookupValue,
  addBinding,
  addValueBinding,
  addTydef,
) where

import Control.Lens hiding (Const)
import Data.Bool (bool)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.Hashable (Hashable, hash)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Lens (setOf)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Language.Context (Ctx)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Key (KeyCombo, prettyKeyCombo)
import Swarm.Language.Requirements.Type (ReqCtx, Requirements)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction
import Swarm.Language.Typed
import Swarm.Language.Types (Polytype, TCtx, TDCtx, TydefInfo, Type, addBindingTD, emptyTDCtx)
import Swarm.Pretty (prettyText)
import Prelude hiding (Foldable (..))

-- | A /value/ is a term that cannot (or does not) take any more
--   evaluation steps on its own.
data Value where
  -- | The unit value.
  VUnit :: Value
  -- | An integer.
  VInt :: Integer -> Value
  -- | Literal text.
  VText :: Text -> Value
  -- | A direction.
  VDir :: Direction -> Value
  -- | A boolean.
  VBool :: Bool -> Value
  -- | A reference to a robot.
  VRobot :: Int -> Value
  -- | An injection into a sum type.  False = left, True = right.
  VInj :: Bool -> Value -> Value
  -- | A pair.
  VPair :: Value -> Value -> Value
  -- | A /closure/, representing a lambda term along with an
  --   environment containing bindings for any free variables in the
  --   body of the lambda.
  VClo :: Var -> Term -> Env -> Value
  -- | An application of a constant to some value arguments,
  --   potentially waiting for more arguments.  If a constant
  --   application is fully saturated (as defined by its 'arity'),
  --   whether it is a value or not depends on whether or not it
  --   represents a command (as defined by 'isCmd').  If a command
  --   (e.g. 'Build'), it is a value, and awaits an 'Swarm.Game.CESK.FExec' frame
  --   which will cause it to execute.  Otherwise (e.g. 'If'), it is
  --   not a value, and will immediately reduce.
  VCApp :: Const -> [Value] -> Value
  -- | An unevaluated bind expression, waiting to be executed, of the
  --   form /i.e./ @c1 ; c2@ or @x <- c1; c2@.  We also store an 'Env'
  --   in which to interpret the commands.
  VBind :: Maybe Var -> Maybe Polytype -> Maybe Requirements -> Term -> Term -> Env -> Value
  -- | A (non-recursive) delayed term, along with its environment. If
  --   a term would otherwise be evaluated but we don't want it to be
  --   (/e.g./ as in the case of arguments to an 'if', or a recursive
  --   binding), we can stick a 'TDelay' on it, which turns it into a
  --   value.  Delayed terms won't be evaluated until 'Force' is
  --   applied to them.
  VDelay :: Term -> Env -> Value
  -- | A reference to a memory cell in the store.
  VRef :: Int -> Value
  -- | An indirection to a value stored in a memory cell.  The
  --   difference between VRef and VIndir is that VRef is a "real"
  --   value (of Ref type), whereas VIndir is just a placeholder.  If
  --   a VRef is encountered during evaluation, it is the final
  --   result; if VIndir is encountered during evaluation, the value
  --   it points to should be looked up.
  VIndir :: Int -> Value
  -- | A record value.
  VRcd :: Map Var Value -> Value
  -- | A keyboard input.
  VKey :: KeyCombo -> Value
  -- | A 'requirements' command awaiting execution.
  VRequirements :: Text -> Term -> Env -> Value
  -- | A 'suspend' command awaiting execution.
  VSuspend :: Term -> Env -> Value
  -- | A special value representing a program that terminated with
  --   an exception.
  VExc :: Value
  -- | A special value used temporarily as the value for a variable
  --   bound by a recursive let, while its definition is being
  --   evaluated.  If the variable is ever referenced again while its
  --   value is still 'VBlackhole', that means it depends on itself in
  --   a way that would trigger an infinite loop, and we can signal an
  --   error.  (Of course, we
  --   <http://www.lel.ed.ac.uk/~gpullum/loopsnoop.html cannot detect
  --   /all/ infinite loops this way>.)
  VBlackhole :: Value
  -- | A special value used to represent runtime type information
  --   passed to ad-hoc polymorphic functions.
  VType :: Type -> Value
  deriving (Eq, Generic, Hashable)

-- For the lack of Show and Eq instances, see Note [Env Show and Eq
-- instances]

-- | A value context is a mapping from variable names to their runtime
--   values.
type VCtx = Ctx Value

--------------------------------------------------
-- Environments
--------------------------------------------------

-- | An environment is a record that stores relevant information for
--   all the variables currently in scope.
data Env = Env
  { _envTypes :: TCtx
  -- ^ Map variables to their types.
  , _envReqs :: ReqCtx
  -- ^ Map variables to the capabilities required to evaluate/execute
  --   them.
  , _envVals :: VCtx
  -- ^ Map variables to their values.
  , _envTydefs :: TDCtx
  -- ^ Type synonym definitions.
  }
  deriving (Hashable, Generic)

-- A derived `Eq` instance for `Env` can cause exponential blowup (see
-- Note [Env Show and Eq instances]), but we need an `Eq` instance in
-- order to have a `Hashable` instance.  So we just implement equality
-- testing by comparing hashes.
instance Eq Env where
  (==) = (==) `on` hash

-- ~~~~ Note [Env Show and Eq instances]
-- Env contains values, which can be e.g. closures, which
-- contain more Envs.  Normally, in memory, there is a lot of sharing,
-- but when naively printing it out all the sharing is lost and it can
-- generate tons of output.  This is why we intentionally do NOT have
-- a Show instance for Env (hence neither for Value).  Similarly, we
-- do not have a derived Eq instance since it would be incredibly
-- inefficient and there is no good reason to use it.  See #2197.

makeLenses ''Env

emptyEnv :: Env
emptyEnv = Env Ctx.empty Ctx.empty Ctx.empty emptyTDCtx

lookupValue :: Var -> Env -> Maybe Value
lookupValue x e = Ctx.lookup x (e ^. envVals)

addBinding :: Var -> Typed Value -> Env -> Env
addBinding x v = at x ?~ v

-- | Add a binding of a variable to a value *only* (no type and
--   requirements).  NOTE that if we then try to look up the variable
--   name using the `At` instance for `Env`, it will report `Nothing`!
--   `lookupValue` will work though.
addValueBinding :: Var -> Value -> Env -> Env
addValueBinding x v = envVals %~ Ctx.addBinding x v

addTydef :: Text -> TydefInfo -> Env -> Env
addTydef x pty = envTydefs %~ addBindingTD x pty

type instance Index Env = Var
type instance IxValue Env = Typed Value

instance Ixed Env
instance At Env where
  at name = lens getter setter
   where
    getter ctx =
      do
        typ <- Ctx.lookup name (ctx ^. envTypes)
        val <- Ctx.lookup name (ctx ^. envVals)
        req <- Ctx.lookup name (ctx ^. envReqs)
        return $ Typed val typ req
    setter ctx Nothing =
      ctx
        & envTypes
          %~ Ctx.delete name
        & envVals
          %~ Ctx.delete name
        & envReqs
          %~ Ctx.delete name
    setter ctx (Just (Typed val typ req)) =
      ctx
        & envTypes
          %~ Ctx.addBinding name typ
        & envVals
          %~ Ctx.addBinding name val
        & envReqs
          %~ Ctx.addBinding name req

------------------------------------------------------------
-- Pretty-printing for values
------------------------------------------------------------

-- | Pretty-print a value.
prettyValue :: Value -> Text
prettyValue = prettyText . valueToTerm

-- | Inject a value back into a term.
valueToTerm :: Value -> Term
valueToTerm = \case
  VUnit -> TUnit
  VInt n -> TInt n
  VText s -> TText s
  VDir d -> TDir d
  VBool b -> TBool b
  VRobot r -> TRobot r
  VInj s v -> TApp (TConst (bool Inl Inr s)) (valueToTerm v)
  VPair v1 v2 -> TPair (valueToTerm v1) (valueToTerm v2)
  VClo x t e ->
    M.foldrWithKey
      ( \y v -> case v of
          VIndir {} -> id
          _ -> TLet LSLet False y Nothing Nothing Nothing (valueToTerm v)
      )
      (TLam x Nothing t)
      (M.restrictKeys (Ctx.unCtx (e ^. envVals)) (S.delete x (setOf freeVarsV (Syntax' NoLoc t Empty ()))))
  VCApp c vs -> foldl' TApp (TConst c) (reverse (map valueToTerm vs))
  VBind mx mty mreq c1 c2 _ -> TBind mx mty mreq c1 c2
  VDelay t _ -> TDelay t
  VRef n -> TRef n
  VIndir n -> TRef n
  VRcd m -> TRcd (Just . valueToTerm <$> m)
  VKey kc -> TApp (TConst Key) (TText (prettyKeyCombo kc))
  VRequirements x t _ -> TRequirements x t
  VSuspend t _ -> TSuspend t
  VExc -> TConst Undefined
  VBlackhole -> TConst Undefined
  VType _ -> TConst Undefined
