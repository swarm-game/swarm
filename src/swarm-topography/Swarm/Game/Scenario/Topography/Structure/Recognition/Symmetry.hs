{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Symmetry analysis for structure recognizer.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Symmetry where

import Control.Monad (forM_, when)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Game.Scenario.Topography.Placement (Orientation (..), applyOrientationTransformNE)
import Swarm.Game.Scenario.Topography.Structure.Named (recognize)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static (RotationalSymmetry (..), SymmetryAnnotatedGrid (..))
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Language.Syntax.Direction (AbsoluteDir (DSouth, DWest), CoordinateOrientation, getCoordinateOrientation)
import Swarm.Util (commaList, histogram, showT)

data RedundantOrientations
  = TwoFoldRedundancy (NonEmpty CoordinateOrientation)
  | FourFoldRedundancy (Set AbsoluteDir)

renderRedundancy :: RedundantOrientations -> T.Text
renderRedundancy = \case
  TwoFoldRedundancy redundantOrientations ->
    T.unwords
      [ "Redundant"
      , commaList $ map showT $ NE.toList redundantOrientations
      , "orientations supplied with two-fold symmetry."
      ]
  FourFoldRedundancy _xs ->
    T.unwords
      [ "Redundant orientations supplied; with four-fold symmetry, just supply 'north'."
      ]

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
  Eq a =>
  ExtractedArea a b ->
  Either RedundantOrientations (SymmetryAnnotatedGrid (ExtractedArea a b))
checkSymmetry x@(ExtractedArea origObject originalRows) = do
  case symmetryType of
    FourFold ->
      when (Set.size suppliedOrientations > 1) . Left $
        FourFoldRedundancy suppliedOrientations
    TwoFold ->
      forM_ (NE.nonEmpty redundantOrientations) $
        Left . TwoFoldRedundancy
     where
      redundantOrientations =
        map fst
          . filter ((> 1) . snd)
          . M.toList
          . histogram
          . map getCoordinateOrientation
          $ Set.toList suppliedOrientations
    _ -> return ()

  return $ SymmetryAnnotatedGrid symmetryType x
 where
  symmetryType
    | quarterTurnRows == originalRows = FourFold
    | halfTurnRows == originalRows = TwoFold
    | otherwise = NoSymmetry

  quarterTurnRows = applyOrientationTransformNE (Orientation DWest False) originalRows
  halfTurnRows = applyOrientationTransformNE (Orientation DSouth False) originalRows

  suppliedOrientations = recognize origObject
