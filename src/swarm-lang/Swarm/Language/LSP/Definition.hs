-- | LSP support for finding and jumping to definitions
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.LSP.Definition (
  findDefinition,
) where

import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text.Utf16.Rope.Mixed qualified as R
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.VFS (VirtualFile (VirtualFile), virtualFileText)
import Swarm.Language.LSP.Position qualified as P
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Pipeline (processParsedTerm)
import Swarm.Language.Syntax (LocVar, Located (..), SrcLoc, Syntax, Syntax' (Syntax'), Term' (..), Var)
import Swarm.Language.TDVar (tdVarName)
import Swarm.Language.Types (Polytype)

findDefinition ::
  J.NormalizedUri ->
  J.Position ->
  VirtualFile ->
  ([J.Range], [Syntax' Polytype])
findDefinition _ p vf@(VirtualFile _ _ myRope) =
  either
    (const ([], []))
    ( \t -> case t of
        Nothing -> ([], [])
        Just t' -> findDef t'
    )
    (readTerm' defaultParserConfig content)
 where
  content = virtualFileText vf
  absolutePos =
    R.charLength . fst $ R.charSplitAtPosition (P.lspToRopePosition p) myRope

  findDef :: Syntax -> ([LSP.Range], [Syntax' Polytype])
  findDef stx =
    case processParsedTerm stx of
      Left _e -> ([], [])
      Right pt -> do
        let path = P.pathToPosition pt $ fromIntegral absolutePos

        -- The last element in the path is the thing we are looking for
        -- get it's name
        let usage = usageName $ NE.last path
        case usage of
          Nothing -> ([], NE.toList path)
          Just u -> do
            let pathTerms = concatMap syntaxVars $! (NE.drop 1 . NE.reverse $ path)
            (mapMaybe (maybeDefPosition u) pathTerms, NE.toList path)

  -- take a syntax element that we want to find the defintion for and
  -- a possible syntax element that contains it's defintion
  -- if this is the matching definition return the position
  maybeDefPosition :: Var -> (SrcLoc, Var) -> Maybe LSP.Range
  maybeDefPosition name' (pos, name)
    | name == name' = P.posToRange myRope pos
    | otherwise = Nothing

-- | find the name of the syntax element if it is a value level variable
-- TODO if we want to support more jump to definitions we should extend this
usageName :: Syntax' a -> Maybe Var
usageName (Syntax' _ (TVar name) _ _) = Just name
usageName _ = Nothing

syntaxVars :: Syntax' a -> [(SrcLoc, Var)]
syntaxVars (Syntax' _ t _ _) = case t of
  (SLet _ _ lv _ _ _ _ _) -> [lvToLoc lv]
  (STydef lv _ _ _) -> [(lvSrcLoc lv, tdVarName $ locVal lv)]
  (SApp s1 _) -> syntaxVars s1
  (SLam lv _ _) -> [lvToLoc lv]
  (SPair s1 s2) -> syntaxVars s1 ++ syntaxVars s2
  (SBind mLV _ _ _ _ _) -> maybeToList (lvToLoc <$> mLV)
  (SDelay s) -> syntaxVars s
  (SRcd m) -> foldr foldRecord [] m
  SProj {} -> mempty
  SAnnotate {} -> mempty
  SSuspend {} -> mempty
  SParens {} -> mempty
  (SRequirements _ _) -> mempty
  TVar {} -> mempty
  TUnit {} -> mempty
  TConst {} -> mempty
  TDir {} -> mempty
  TInt {} -> mempty
  TAntiInt {} -> mempty
  TText {} -> mempty
  TAntiText {} -> mempty
  TAntiSyn {} -> mempty
  TBool {} -> mempty
  TRobot {} -> mempty
  TRef {} -> mempty
  TRequire {} -> mempty
  TStock {} -> mempty
  TType {} -> mempty
 where
  lvToLoc lv = (lvSrcLoc lv, locVal lv)

  foldRecord (lv, ms) acc = lvToLoc lv : (maybe [] syntaxVars ms ++ acc)
