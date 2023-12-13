{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Symmetry analysis for structure recognizer.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Symmetry where

import Control.Monad (unless, when)
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Game.Scenario.Topography.Placement (Orientation (..), applyOrientationTransform)
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type (RotationalSymmetry (..), SymmetryAnnotatedGrid (..))
import Swarm.Language.Direction (AbsoluteDir (DSouth, DWest), getCoordinateOrientation)
import Swarm.Util (commaList, failT, histogram, showT)

-- | Warns if any recognition orientations are redundant
-- by rotational symmetry.
-- We can accomplish this by testing only two rotations:
--
-- 1. Rotate 90 degrees. If identical to the original
--    orientation, then has 4-fold symmetry and we don't
--    need to check any other orientations.
--    Warn if more than one recognition orientation was supplied.
-- 2. Rotate 180 degrees.  At best, we may now have
--    2-fold symmetry.
--    Warn if two opposite orientations were supplied.
checkSymmetry ::
  (MonadFail m, Eq a) => Structure.NamedGrid a -> m (SymmetryAnnotatedGrid a)
checkSymmetry ng = do
  case symmetryType of
    FourFold ->
      when (Set.size suppliedOrientations > 1)
        . failT
        . pure
        $ T.unwords ["Redundant orientations supplied; with four-fold symmetry, just supply 'north'."]
    TwoFold ->
      unless (null redundantOrientations)
        . failT
        . pure
        $ T.unwords
          [ "Redundant"
          , commaList $ map showT redundantOrientations
          , "orientations supplied with two-fold symmetry."
          ]
     where
      redundantOrientations =
        map fst
          . filter ((> 1) . snd)
          . M.toList
          . histogram
          . map getCoordinateOrientation
          $ Set.toList suppliedOrientations
    _ -> return ()

  return $ SymmetryAnnotatedGrid ng symmetryType
 where
  symmetryType
    | quarterTurnRows == originalRows = FourFold
    | halfTurnRows == originalRows = TwoFold
    | otherwise = NoSymmetry

  quarterTurnRows = applyOrientationTransform (Orientation DWest False) originalRows
  halfTurnRows = applyOrientationTransform (Orientation DSouth False) originalRows

  suppliedOrientations = Structure.recognize ng
  Structure.Grid originalRows = Structure.structure ng
