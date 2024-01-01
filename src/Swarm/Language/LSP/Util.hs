module Swarm.Language.LSP.Util where

import Data.Text.Utf16.Rope qualified as R
import Language.LSP.Types qualified as J
import Swarm.Language.Syntax

posToRange :: R.Rope -> SrcLoc -> Maybe J.Range
posToRange myRope foundSloc = do
  (s, e) <- case foundSloc of
    SrcLoc s e -> Just (s, e)
    _ -> Nothing
  (startRope, _) <- R.splitAt (fromIntegral s) myRope
  (endRope, _) <- R.splitAt (fromIntegral e) myRope
  return $
    J.Range
      (ropeToLspPosition $ R.lengthAsPosition startRope)
      (ropeToLspPosition $ R.lengthAsPosition endRope)


ropeToLspPosition :: R.Position -> J.Position
ropeToLspPosition (R.Position l c) =
  J.Position (fromIntegral l) (fromIntegral c)

lspToRopePosition :: J.Position -> R.Position
lspToRopePosition (J.Position myLine myCol) =
  R.Position (fromIntegral myLine) (fromIntegral myCol)
