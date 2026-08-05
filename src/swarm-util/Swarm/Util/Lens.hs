-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Lens generation utilities.
module Swarm.Util.Lens (
  makeLensesNoSigs,
  makeLensesExcluding,
  inherit,
  concatFold,
  view,
  (+=),
  (.=),
  use,
  uses,
  (%=),
) where

import Control.Lens (
  Fold,
  Getting,
  Lens',
  folding,
  generateSignatures,
  lensField,
  lensRules,
  makeLensesWith,
  mapped,
  view,
  (%~),
  (&),
  (.~),
  (^.),
  (^..),
 )

import Control.Lens qualified as L
import Effectful
import Effectful.State.Static.Local
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

-- | Copy a given field from one record to another.
inherit :: Lens' s a -> s -> (s -> s)
inherit field parent child = child & field .~ (parent ^. field)

-- | Concatenate two folds into a single fold which encompasses all
--   elements from both.
concatFold :: Fold s a -> Fold s a -> Fold s a
concatFold f1 f2 = folding (\s -> (s ^.. f1) ++ (s ^.. f2))

-- | Get the target of a 'Lens' or @Getter
use :: State s :> es => Getting a s a -> Eff es a
use l = gets (L.view l)

-- | Variant of use that applies the given function to the target
uses :: State s :> es => Getting a s a -> (a -> b) -> Eff es b
uses l f = f <$> gets (L.view l)

infixr 4 .=, %=, +=

-- | Replace the target of the given 'Lens' (or all the targets of a @Setter@ or 'Traversal') in the current monadic state
(.=) :: State s :> es => L.ASetter s s a b -> b -> Eff es ()
l .= b = modify (L.set l b)

-- | Map over the target of the given 'Lens' (or all the targets of a @Setter@ or 'Traversal') in the current monadic state
(%=) :: State s :> es => L.ASetter s s a b -> (a -> b) -> Eff es ()
l %= f = modify (L.over l f)

-- | Modify the target(s) of the given 'Lens' by adding a value
(+=) :: (State s :> es, Num a) => L.ASetter' s a -> a -> Eff es ()
l += v = modify (l L.+~ v)
