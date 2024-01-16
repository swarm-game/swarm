{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent robot context.
module Swarm.Game.Robot.Context where

import Control.Lens hiding (Const, contains)
import Data.Aeson qualified as Ae (FromJSON, ToJSON (..))
import GHC.Generics (Generic)
import Swarm.Game.CESK qualified as C
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Requirement (ReqCtx)
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Types (TCtx)
import Swarm.Language.Value as V

-- | A record that stores the information
--   for all definitions stored in a 'Robot'
data RobotContext = RobotContext
  { _defTypes :: TCtx
  -- ^ Map definition names to their types.
  , _defReqs :: ReqCtx
  -- ^ Map definition names to the capabilities
  --   required to evaluate/execute them.
  , _defVals :: Env
  -- ^ Map definition names to their values. Note that since
  --   definitions are delayed, the values will just consist of
  --   'VRef's pointing into the store.
  , _defStore :: C.Store
  -- ^ A store containing memory cells allocated to hold
  --   definitions.
  }
  deriving (Eq, Show, Generic, Ae.FromJSON, Ae.ToJSON)

makeLenses ''RobotContext

emptyRobotContext :: RobotContext
emptyRobotContext = RobotContext Ctx.empty Ctx.empty Ctx.empty C.emptyStore

type instance Index RobotContext = Ctx.Var
type instance IxValue RobotContext = Typed Value

instance Ixed RobotContext
instance At RobotContext where
  at name = lens getter setter
   where
    getter ctx =
      do
        typ <- Ctx.lookup name (ctx ^. defTypes)
        val <- Ctx.lookup name (ctx ^. defVals)
        req <- Ctx.lookup name (ctx ^. defReqs)
        return $ Typed val typ req
    setter ctx Nothing =
      ctx
        & defTypes %~ Ctx.delete name
        & defVals %~ Ctx.delete name
        & defReqs %~ Ctx.delete name
    setter ctx (Just (Typed val typ req)) =
      ctx
        & defTypes %~ Ctx.addBinding name typ
        & defVals %~ Ctx.addBinding name val
        & defReqs %~ Ctx.addBinding name req
