{-# LANGUAGE OverloadedStrings #-}

-- LSP support for finding and jumping to definitions
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.LSP.Definition (
  findDefinition,
  DefinitionResult (..),
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope.Mixed qualified as R
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.VFS (VirtualFile (VirtualFile), virtualFileText)
import Swarm.Language.LSP.Position qualified as P
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (ParserError, defaultParserConfig)
import Swarm.Language.Pipeline (processParsedTerm)
import Swarm.Language.Syntax (Located (..), SrcLoc, Syntax, Syntax' (Syntax'), Term' (..), Var)
import Swarm.Language.TDVar (tdVarName)
import Swarm.Language.Typecheck (ContextualTypeErr)
import System.IO (stderr)

data DefinitionResult = TError ContextualTypeErr | PError ParserError | Unsupported | NotFound | Found [J.Range]

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
  findDef :: Syntax -> DefinitionResult
  findDef stx =
    case processParsedTerm stx of
      Left e -> TError e
      Right pt -> do
        let path = P.pathToPosition pt $ fromIntegral absolutePos
        let usage = usageName $ NE.last path

        case usage of
          Nothing -> Unsupported
          Just u -> do
            let pathTerms = concatMap syntaxVars (NE.drop 1 . NE.reverse $ path)
            case mapMaybe (maybeDefPosition u) pathTerms of
              [] -> NotFound
              ranges -> Found ranges

  -- take a syntax element that we want to find the defintion for and
  -- a possible syntax element that contains it's defintion
  -- if this is the matching definition return the position
  maybeDefPosition :: Var -> (SrcLoc, Var) -> Maybe LSP.Range
  maybeDefPosition name (pos, name')
    | name == name' = P.posToRange myRope pos
    | otherwise = Nothing

debug :: (MonadIO m) => Text -> m ()
debug msg = liftIO $ Text.hPutStrLn stderr $ "[swarm-lsp] " <> msg

-- | find the name of the syntax element if it is a value level variable
usageName :: Syntax' a -> Maybe Var
usageName (Syntax' _ t _ _) = case t of
  (TVar n) -> Just n
  _ -> Nothing

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
