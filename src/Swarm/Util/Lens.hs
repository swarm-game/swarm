-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Lens generation utilities.
module Swarm.Util.Lens (
  makeLensesNoSigs,
  makeLensesExcluding,
) where

import Control.Lens (
  generateSignatures,
  lensField,
  lensRules,
  makeLensesWith,
  mapped,
  (%~),
  (&),
  (.~),
 )
import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (Name)

-- | Generate lenses but with no type signatures, so we can explicitly
--   give type signatures and attach custom Haddock documentation to
--   them.
makeLensesNoSigs :: Name -> DecsQ
makeLensesNoSigs = makeLensesWith (lensRules & generateSignatures .~ False)

-- | Generate lenses for the fields of a record type (with no type
--   signatures), except for a given list of excluded fields.
--
--   Especially useful in conjunction with the design pattern
--   described in
--   https://byorgey.wordpress.com/2021/09/17/automatically-updated-cached-views-with-lens/
makeLensesExcluding :: [Name] -> Name -> DecsQ
makeLensesExcluding exclude =
  makeLensesWith
    ( lensRules
        & generateSignatures .~ False
        & lensField . mapped . mapped %~ \fn n ->
          if n `elem` exclude then [] else fn n
    )
