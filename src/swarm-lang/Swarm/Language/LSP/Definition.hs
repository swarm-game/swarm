-- LSP support for finding and jumping to definitions
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.LSP.Definition (
  findDefinition,
  DefinitionResult (..),
)
where

import Data.Foldable (find)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text.Utf16.Rope.Mixed qualified as R
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.VFS (VirtualFile (VirtualFile), virtualFileText)
import Swarm.Language.LSP.Position qualified as P
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (ParserError, defaultParserConfig)
import Swarm.Language.Syntax (Located (..), Phase (..), SrcLoc, Syntax (..), Term (..), Var)
import Swarm.Language.Typecheck (ContextualTypeErr)

data DefinitionResult = TError ContextualTypeErr | PError ParserError | Unsupported | NotFound | Found J.Range

findDefinition ::
  J.NormalizedUri ->
  J.Position ->
  VirtualFile ->
  DefinitionResult
findDefinition _ p vf@(VirtualFile _ _ myRope) =
  either
    PError
    (maybe Unsupported findDef)
    (readTerm' defaultParserConfig content)
 where
  content = virtualFileText vf
  absolutePos =
    R.charLength . fst $ R.charSplitAtPosition (P.lspToRopePosition p) myRope

  -- build a list from the syntax tree and starting from the position of the cursor (the bottom).
  -- search for the matching definition.
  findDef :: Syntax Raw -> DefinitionResult
  findDef pt = do
    let path = P.pathToPosition pt $ fromIntegral absolutePos
    let usage = usageName $ NE.last path

    case usage of
      Nothing -> Unsupported
      Just u -> do
        let pathTerms = mapMaybe boundVars (NE.drop 1 . NE.reverse $ path)
        maybe NotFound Found (find (\t -> u == snd t) pathTerms >>= P.posToRange myRope . fst)

-- | find the name of the syntax element if it is a value level variable
usageName :: Syntax phase -> Maybe Var
usageName (Syntax _ t _ _) = case t of
  (TVar n) -> Just n
  _ -> Nothing

boundVars :: Syntax phase -> Maybe (SrcLoc, Var)
boundVars (Syntax _ t _ _) = case t of
  (SLet _ _ lv _ _ _ _ _) -> Just $ lvToLoc lv
  (SLam lv _ _) -> Just $ lvToLoc lv
  (SBind mLV _ _ _ _ _) -> lvToLoc <$> mLV
  STydef {} -> mempty
  SApp {} -> mempty
  SPair {} -> mempty
  SDelay {} -> mempty
  SRcd {} -> mempty
  SProj {} -> mempty
  SAnnotate {} -> mempty
  SSuspend {} -> mempty
  SParens {} -> mempty
  (SRequirements _ _) -> mempty
  SImportIn {} -> mempty
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
