{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for terms of the Swarm programming language.
module Swarm.Language.Syntax (
  -- * Constants
  Const (..),
  allConst,
  ConstInfo (..),
  ConstDoc (..),
  ConstMeta (..),
  MBinAssoc (..),
  MUnAssoc (..),
  constInfo,
  arity,
  isCmd,
  isUserFunc,
  isOperator,
  isBuiltinFunction,
  isTangible,
  isLong,

  -- * Size limits
  maxSniffRange,
  maxScoutRange,
  maxStrideRange,
  maxPathRange,
  globalMaxVolume,

  -- * SrcLoc
  SrcLoc (..),
  srcLocStartsBefore,
  srcLocEndsBefore,
  noLoc,

  -- * Comments
  CommentType (..),
  CommentSituation (..),
  isStandalone,
  Comment (..),
  Comments (..),
  beforeComments,
  afterComments,

  -- * Phase
  Phase (..),
  ImportPhaseFor,
  SwarmType,
  Anchor,
  Unresolvable,

  -- * Imports
  ImportDir,
  ImportLoc (..),
  locToFilePath,

  -- * Syntax
  Syntax (..),
  sLoc,
  sTerm,
  sType,
  sComments,
  pattern RSyntax,
  pattern CSyntax,
  Located (..),
  LocVar,
  LetSyntax (..),
  pattern RTerm,
  pattern TRequirements,
  pattern TPair,
  pattern TLam,
  pattern TApp,
  pattern (:$:),
  pattern TLet,
  pattern TTydef,
  pattern TBind,
  pattern TDelay,
  pattern TRcd,
  pattern TProj,
  pattern TAnnotate,
  pattern TSuspend,
  pattern TImportIn,
  pattern TParens,

  -- * Terms
  Var,
  Term (..),
  mkOp,
  mkOp',
  unfoldApps,
  mkTuple,
  unTuple,
  locVarToSyntax,

  -- * Traversals

  -- ** Term + type traversal
  termSyntax,
  traverseSyntax,

  -- ** Erasure
  erase,
  eraseRaw,

  -- ** Free variable traversal
  freeVarsS,
  freeVarsT,
  freeVarsV,
  mapFreeS,

  -- ** Miscellaneous traversals
  asTree,
  measureAstSize,
) where

import Swarm.Language.Phase
import Swarm.Language.Syntax.AST
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Import
import Swarm.Language.Syntax.Loc
import Swarm.Language.Syntax.Pattern
import Swarm.Language.Syntax.Pretty ()
import Swarm.Language.Syntax.Util
import Swarm.Language.Types
import Swarm.Language.Var (LocVar)
