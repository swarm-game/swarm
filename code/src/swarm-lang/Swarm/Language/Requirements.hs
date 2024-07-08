-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Requirements are things that are needed in order to successfully
-- build a robot running a certain program.
module Swarm.Language.Requirements (
  -- * Requirements

  -- ** The 'Requirement' type
  Requirement (..),

  -- ** The 'Requirements' type and utility functions
  Requirements (..),
  singleton,
  singletonCap,
  singletonDev,
  singletonInv,
  insert,
  ReqCtx,

  -- * Requirements analysis
  requirements,
) where

import Swarm.Language.Requirements.Analysis
import Swarm.Language.Requirements.Type
