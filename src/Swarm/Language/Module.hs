-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A 'Module' packages together a type-annotated syntax tree along
-- with a context of top-level definitions.
module Swarm.Language.Module (
  -- * Modules
  Module (..),
  TModule,
  UModule,
  trivMod,
) where

import Data.Data (Data)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Swarm.Language.Context (Ctx, empty)
import Swarm.Language.Syntax (Syntax')
import Swarm.Language.Types (Polytype, UPolytype, UType)

------------------------------------------------------------
-- Modules
------------------------------------------------------------

-- | A module generally represents the result of performing type
--   inference on a top-level expression, which in particular can
--   contain definitions ('Swarm.Language.Syntax.TDef').  A module
--   contains the type-annotated AST of the expression itself, as well
--   as the context giving the types of any defined variables.
data Module s t = Module {moduleAST :: Syntax' s, moduleCtx :: Ctx t}
  deriving (Show, Eq, Functor, Data, Generic, FromJSON, ToJSON)

-- | A 'TModule' is the final result of the type inference process on
--   an expression: we get a polytype for the expression, and a
--   context of polytypes for the defined variables.
type TModule = Module Polytype Polytype

-- | A 'UModule' represents the type of an expression at some
--   intermediate stage during the type inference process.  We get a
--   'UType' (/not/ a 'UPolytype') for the expression, which may
--   contain some free unification or type variables, as well as a
--   context of 'UPolytype's for any defined variables.
type UModule = Module UType UPolytype

-- | The trivial module for a given AST, with the empty context.
trivMod :: Syntax' s -> Module s t
trivMod t = Module t empty
