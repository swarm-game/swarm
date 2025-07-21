{-# LANGUAGE OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause
-- Finding lsp positions and mapping between positions and ropes
module Swarm.Language.LSP.Position (
  narrowToPosition,
  pathToPosition,
  lspToRopePosition,
  posToRange,
  ExplainableType,
  prettyType,
  eq,
  getInnerType,
)
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Lines qualified as R
import Data.Text.Utf16.Rope.Mixed qualified as R
import Language.LSP.Protocol.Types qualified as J
import Swarm.Language.Syntax (SwarmType, locVarToSyntax)
import Swarm.Language.Syntax.AST (Syntax (..), Term (..))
import Swarm.Language.TDVar (tdVarName)
import Swarm.Language.Types
import Swarm.Pretty (prettyTextLine)
import Swarm.Util.SrcLoc (SrcLoc (..))

posToRange :: R.Rope -> SrcLoc -> Maybe J.Range
posToRange myRope foundSloc = do
  (s, e) <- case foundSloc of
    SrcLoc s e -> Just (s, e)
    _ -> Nothing
  let (startRope, _) = R.charSplitAt (fromIntegral s) myRope
      (endRope, _) = R.charSplitAt (fromIntegral e) myRope
  return $
    J.Range
      (ropeToLspPosition $ R.charLengthAsPosition startRope)
      (ropeToLspPosition $ R.charLengthAsPosition endRope)

ropeToLspPosition :: R.Position -> J.Position
ropeToLspPosition (R.Position l c) =
  J.Position (fromIntegral l) (fromIntegral c)

lspToRopePosition :: J.Position -> R.Position
lspToRopePosition (J.Position myLine myCol) =
  R.Position (fromIntegral myLine) (fromIntegral myCol)

class (Show t) => ExplainableType t where
  fromPoly :: Polytype -> t

  -- | Pretty print the type.
  prettyType :: t -> Text

  -- | Strip the type of its outermost layer.
  --
  -- This allows us to strip lambda or command type
  -- and get the type of the bound variable.
  getInnerType :: t -> t

  -- | Check if this type is same as the given 'Polytype'.
  --
  -- We use it to not print same type twice (e.g. inferred and generic).
  eq :: t -> Polytype -> Bool

instance ExplainableType () where
  fromPoly = const ()
  prettyType = const "?"
  getInnerType = id
  eq _ _ = False

instance ExplainableType Polytype where
  fromPoly = id
  prettyType = prettyTextLine
  getInnerType = fmap $ \case
    (l :->: _r) -> l
    (TyCmd t) -> t
    t -> t
  eq = (==)

instance ExplainableType RawPolytype where
  fromPoly = forgetQ
  prettyType = prettyTextLine
  getInnerType = fmap $ \case
    (l :->: _r) -> l
    (TyCmd t) -> t
    t -> t
  eq r t = r == forgetQ t

-- | Find the most specific term for a given
-- position within the code.
narrowToPosition ::
  ExplainableType (SwarmType phase) =>
  -- | parent term
  Syntax phase ->
  -- | absolute offset within the file.
  Int ->
  Syntax phase
narrowToPosition s i = NE.last $ pathToPosition s i

-- | Find the most specific term for a given
-- position within the code, recording the terms along the way for later processing.

-- The list is nonempty because at minimum we can return the element of the syntax we are currently processing.
pathToPosition ::
  forall phase.
  ExplainableType (SwarmType phase) =>
  -- | parent term
  Syntax phase ->
  -- | absolute offset within the file
  Int ->
  NonEmpty (Syntax phase)
pathToPosition s0 pos = s0 :| fromMaybe [] (innerPath s0)
 where
  innerPath :: Syntax phase -> Maybe [Syntax phase]
  innerPath (Syntax _ t _ ty) = case t of
    SLam lv _ s -> d (locVarToSyntax lv $ getInnerType ty) <|> d s
    SApp s1 s2 -> d s1 <|> d s2
    SLet _ _ lv _ _ _ s1@(Syntax _ _ _ lty) s2 -> d (locVarToSyntax lv lty) <|> d s1 <|> d s2
    SBind mlv _ _ _ s1@(Syntax _ _ _ lty) s2 -> (mlv >>= d . flip locVarToSyntax (getInnerType lty)) <|> d s1 <|> d s2
    STydef typ typBody _ti s1 -> d s1 <|> Just [locVarToSyntax (tdVarName <$> typ) $ fromPoly typBody]
    SPair s1 s2 -> d s1 <|> d s2
    SDelay s -> d s
    SRcd m -> asum . map d . mapMaybe snd $ m
    SProj s1 _ -> d s1
    SAnnotate s _ -> d s
    SRequirements _ s -> d s
    SParens s -> d s
    -- atoms - return their position and end recursion
    TUnit -> mempty
    TConst {} -> mempty
    TDir {} -> mempty
    TInt {} -> mempty
    TText {} -> mempty
    TBool {} -> mempty
    TVar {} -> mempty
    TStock {} -> mempty
    TRequire {} -> mempty
    TType {} -> mempty
    -- these should not show up in surface language
    TRef {} -> mempty
    TRobot {} -> mempty
    TAntiInt {} -> mempty
    TAntiText {} -> mempty
    TAntiSyn {} -> mempty
    SSuspend {} -> mempty

  d = descend pos
  -- try and decend into the syntax element if it is contained with position
  descend ::
    ExplainableType (SwarmType phase) =>
    Int ->
    Syntax phase ->
    Maybe [Syntax phase]
  descend p s1@(Syntax l1 _ _ _) = do
    guard $ withinBound p l1
    pure $ case innerPath s1 of
      Nothing -> [s1]
      Just iss -> s1 : iss

withinBound :: Int -> SrcLoc -> Bool
withinBound pos (SrcLoc _ s e) = pos >= s && pos < e
withinBound _ NoLoc = False
