{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Language Server Protocol (LSP) server for the Swarm language.
-- See the docs/EDITORS.md to learn how to use it.
module Swarm.Language.LSP where

import Control.Lens (to, (^.))
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Int (Int32)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message (TResponseError (TResponseError))
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (ErrorCodes (..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.VFS (VirtualFile (..), virtualFileText)
import Swarm.Failure (SystemFailure (..))
import Swarm.Language.LSP.Definition qualified as D
import Swarm.Language.LSP.Hover qualified as H
import Swarm.Language.LSP.VarUsage qualified as VU
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Parser.Util (getLocRange, showErrorPos)
import Swarm.Language.Pipeline (processTerm)
import Swarm.Language.Syntax (erase, eraseRaw)
import Swarm.Language.Syntax.Loc (SrcLoc (NoLoc, SrcLoc))
import Swarm.Language.Typecheck (ContextualTypeErr (..))
import Swarm.Language.Value (emptyEnv)
import Swarm.Pretty (prettyText)
import System.IO (stderr)
import Witch

lspMain :: IO ()
lspMain =
  void $
    runServer $
      ServerDefinition
        { defaultConfig = ()
        , configSection = "swarm"
        , parseConfig = const $ const $ Right ()
        , onConfigChange = const $ return ()
        , doInitialize = \env _req -> pure $ Right env
        , staticHandlers = const handlers
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options =
            defaultOptions
              { -- set sync options to get DidSave event, as well as Open and Close events.
                optTextDocumentSync =
                  Just
                    ( LSP.TextDocumentSyncOptions
                        (Just True)
                        (Just syncKind)
                        (Just False)
                        (Just False)
                        (Just . LSP.InR . LSP.SaveOptions $ Just True)
                    )
              }
        }
 where
  -- Using SyncFull seems to handle the debounce for us.
  -- The alternative is to use SyncIncremental, but then the
  -- handler is called for each keystroke.
  syncKind = LSP.TextDocumentSyncKind_Full

diagnosticSourcePrefix :: Text
diagnosticSourcePrefix = "swarm-lsp"

debug :: (MonadIO m) => Text -> m ()
debug msg = liftIO $ Text.hPutStrLn stderr $ "[swarm-lsp] " <> msg

type TextDocumentVersion = Int32

validateSwarmCode :: LSP.NormalizedUri -> Maybe TextDocumentVersion -> Text -> LspM () ()
validateSwarmCode doc version content = do
  -- debug $ "Validating: " <> from (show doc) <> " ( " <> content <> ")"

  -- FIXME: #1040 With this call to flushDiagnosticsBySource in place, the warnings
  -- in other buffers (editor tabs) end up getting cleared when switching between
  -- (focusing on) other buffers in VS Code.
  -- However, getting rid of this seems to break error highlighting.
  flushDiagnosticsBySource 0 (Just diagnosticSourcePrefix)

  res <- liftIO $ processTerm content
  let (errors, warnings) = case res of
        Right Nothing -> ([], [])
        Right (Just term) -> ([], unusedWarnings)
         where
          VU.Usage _ problems = VU.getUsage mempty (eraseRaw term)
          unusedWarnings = mapMaybe (VU.toErrPos content) problems
        Left (DoesNotTypecheck l t) -> ([(srcLocToPos l content, t)], [])
        Left err -> ([(((0, 0), (0, 0)), prettyText err)], [])

  publishDiags $
    map makeUnusedVarDiagnostic warnings

  -- NOTE: "publishDiags" keeps only one diagnostic at a
  -- time (the most recent) so we make sure the errors are
  -- issued last (after any warnings).
  -- Note that it does not achieve the desired effect to simply
  -- concatenate the two diagnostic lists into a single
  -- publishDiagnostics function call (regardless of the order
  -- of the lists).
  publishDiags $
    map makeParseErrorDiagnostic errors
 where
  publishDiags :: [LSP.Diagnostic] -> LspM () ()
  publishDiags = publishDiagnostics 1 doc version . partitionBySource

  makeUnusedVarDiagnostic :: (LSP.Range, Text) -> LSP.Diagnostic
  makeUnusedVarDiagnostic (range, msg) =
    LSP.Diagnostic
      range
      (Just LSP.DiagnosticSeverity_Warning) -- severity
      Nothing -- code
      Nothing -- code description
      (Just diagnosticSourcePrefix) -- source
      msg
      (Just [LSP.DiagnosticTag_Unnecessary]) -- tags
      Nothing -- related source code info
      Nothing -- data
  makeParseErrorDiagnostic :: (((Int, Int), (Int, Int)), Text) -> LSP.Diagnostic
  makeParseErrorDiagnostic (((startLine, startCol), (endLine, endCol)), msg) =
    LSP.Diagnostic
      ( LSP.Range
          (LSP.Position (fromIntegral startLine) (fromIntegral startCol))
          (LSP.Position (fromIntegral endLine) (fromIntegral endCol))
      )
      (Just LSP.DiagnosticSeverity_Error) -- severity
      Nothing -- code
      Nothing -- code description
      (Just diagnosticSourcePrefix) -- source
      msg
      Nothing -- tags
      (Just []) -- related info
      Nothing -- data

srcLocToPos :: SrcLoc -> Text -> ((Int, Int), (Int, Int))
srcLocToPos l code = (minusOne start, minusOne end)
 where
  minusOne (x, y) = (x - 1, y - 1)

  (start, end) = case l of
    SrcLoc _ s e -> getLocRange code (s, e)
    NoLoc -> ((1, 1), (65535, 65535)) -- unknown loc spans the whole document

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
        debug "Initialized"
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = fromMaybe "?" $ msg ^. LSP.params . LSP.text
        validateSwarmCode (LSP.toNormalizedUri doc) Nothing content
    , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = msg ^. LSP.params . LSP.textDocument . LSP.text
        validateSwarmCode (LSP.toNormalizedUri doc) Nothing content
    , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri . to LSP.toNormalizedUri
        mdoc <- getVirtualFile doc
        case mdoc of
          Just vf@(VirtualFile _ version _rope) -> do
            validateSwarmCode doc (Just (fromIntegral version)) (virtualFileText vf)
          _ -> debug $ "No virtual file found for: " <> from (show msg)
    , requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
        let doc = req ^. LSP.params . LSP.textDocument . LSP.uri . to LSP.toNormalizedUri
            pos = req ^. LSP.params . LSP.position
        mdoc <- getVirtualFile doc
        let maybeHover = do
              vf <- mdoc
              (markdownText, maybeRange) <- H.showHoverInfo doc pos vf
              return $ LSP.Hover (LSP.InL $ LSP.MarkupContent LSP.MarkupKind_Markdown markdownText) maybeRange
        responder . Right . LSP.maybeToNull $ maybeHover
    , requestHandler LSP.SMethod_TextDocumentDefinition $ \req responder -> do
        let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
            doc = uri ^. to LSP.toNormalizedUri
            pos = req ^. LSP.params . LSP.position
        mdoc <- getVirtualFile doc
        let defs = maybe D.Unsupported (D.findDefinition doc pos) mdoc
        case defs of
          D.Unsupported -> responder . Left $ TResponseError (LSP.InR ErrorCodes_MethodNotFound) "Unsupported find definition" Nothing
          D.PError e -> responder . Left $ TResponseError (LSP.InR ErrorCodes_ParseError) (T.pack $ show e) Nothing
          D.TError e -> responder . Left $ TResponseError (LSP.InR ErrorCodes_ParseError) (T.pack $ show e) Nothing
          D.NotFound -> responder . Right . LSP.InR . LSP.InR $ LSP.Null
          D.Found defs' -> responder . Right . LSP.InL . LSP.Definition . LSP.InL $ LSP.Location uri defs'
    ]
