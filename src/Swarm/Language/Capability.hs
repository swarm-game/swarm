{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Capability
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Capabilities needed to evaluate and execute programs.
--
-----------------------------------------------------------------------------

module Swarm.Language.Capability
  ( Capability(..)
  , requiredCaps
  ) where

import           Data.Hashable         (Hashable)
import           Data.Set              (Set)
import qualified Data.Set              as S
import           GHC.Generics          (Generic)
import           Swarm.Language.Syntax

-- | Various capabilities which robots can have.
data Capability
  = CMove     -- ^ Execute the 'Move' command
  | CTurn     -- ^ Execute the 'Turn' command
  | CGrab     -- ^ Execute the 'Grab' command
  | CPlace    -- ^ Execute the 'Place' command
  | CGive     -- ^ Execute the 'Give' command
  | CCraft    -- ^ Execute the 'Craft' command
  | CBuild    -- ^ Execute the 'Build' command
  | CSenseLoc -- ^ Execute the 'GetX' and 'GetY' commands
  | CRandom   -- ^ Execute the 'Random' command
  | CSay      -- ^ Execute the 'Say' command
  | CAppear   -- ^ Execute the 'Appear' command

  | CCond     -- ^ Evaluate conditional expressions
  | CCmp      -- ^ Evaluate comparison operations
  | CArith    -- ^ Evaluate arithmetic operations
  | CEnv      -- ^ Store and look up definitions in an environment
  | CLam      -- ^ Interpret lambda abstractions
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable)

  -- XXX we could make this a bit more refined, e.g.:
  --   - distinguish between capabilities needed to *evaluate* vs *execute*
  --   - do something more sophisticated for lambdas, let expressions, etc.
  --
  -- i.e. we could actually do abstract interpretation, keeping track
  -- of an environment where each identifier maps to the capabilities
  -- needed to execute it (because of eager evaluation, the
  -- capabilities to evaluate it will already be incurred by the time
  -- it ends up in the environment)


-- | Analyze a program to see what capabilities may be needed to
--   execute it. Note that this is necessarily a conservative
--   analysis, especially if the program contains conditional
--   expressions.  Some capabilities may end up not being actually
--   needed if certain commands end up not being executed.  However,
--   the analysis is safe in the sense that a robot with the indicated
--   capabilities will always be able to run the given program no
--   matter what.
requiredCaps :: Term -> Set Capability
requiredCaps TUnit            = S.empty
requiredCaps (TConst c)       = constCaps c
requiredCaps (TDir _)         = S.empty
requiredCaps (TInt _)         = S.empty
requiredCaps (TString _)      = S.empty
requiredCaps (TBool _)        = S.empty
requiredCaps (TVar _)         = S.empty
requiredCaps (TPair t1 t2)    = requiredCaps t1 `S.union` requiredCaps t2
requiredCaps (TLam _ _ t)     = S.insert CLam $ requiredCaps t
requiredCaps (TApp t1 t2)     = requiredCaps t1 `S.union` requiredCaps t2
requiredCaps (TLet _ _ t1 t2) = S.insert CEnv $ requiredCaps t1 `S.union` requiredCaps t2
requiredCaps (TDef _ _ t)     = S.insert CEnv $ requiredCaps t
requiredCaps (TBind _ t1 t2)  = requiredCaps t1 `S.union` requiredCaps t2
requiredCaps (TDelay t)       = requiredCaps t

-- | Capabilities needed to evaluate/execute a constant.
constCaps :: Const -> Set Capability
constCaps Wait      = S.empty
constCaps Noop      = S.empty
constCaps Halt      = S.empty

constCaps Move      = S.singleton CMove
constCaps Turn      = S.singleton CTurn
constCaps Grab      = S.singleton CGrab
constCaps Place     = S.singleton CPlace
constCaps Give      = S.singleton CGive
constCaps Craft     = S.singleton CCraft
constCaps Build     = S.singleton CBuild
constCaps Say       = S.singleton CSay
constCaps View      = S.empty
constCaps Appear    = S.singleton CAppear

constCaps GetX      = S.singleton CSenseLoc
constCaps GetY      = S.singleton CSenseLoc
constCaps Ishere    = S.empty
constCaps Random    = S.singleton CRandom

constCaps Run       = S.empty

constCaps Not       = S.empty
constCaps (Cmp _)   = S.singleton CCmp
constCaps Neg       = S.singleton CArith
constCaps (Arith _) = S.singleton CArith

constCaps If        = S.singleton CCond
constCaps Fst       = S.empty
constCaps Snd       = S.empty
constCaps Force     = S.empty
constCaps Return    = S.empty
constCaps Try       = S.empty
constCaps Raise     = S.empty
