{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Swarm.Language.LSP
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Language Server Protocol (LSP) server for the Swarm language.
-- See the docs/EDITORS.md to learn how to use it.
module Swarm.Language.LSP where

import Control.Lens (to, (^.))
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Language.LSP.Diagnostics
import Language.LSP.Server
import Language.LSP.Types (Hover (Hover))
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import Language.LSP.VFS
import Swarm.Language.LSP.Hover qualified as H
import Swarm.Language.LSP.VarUsage qualified as VU
import Swarm.Language.Parse
import Swarm.Language.Pipeline
import System.IO (stderr)
import Witch

lspMain :: IO ()
lspMain =
  void $
    runServer $
      ServerDefinition
        { onConfigurationChange = const $ const $ Right ()
        , defaultConfig = ()
        , doInitialize = \env _req -> pure $ Right env
        , staticHandlers = handlers
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options =
            defaultOptions
              { -- set sync options to get DidSave event, as well as Open and Close events.
                textDocumentSync =
                  Just
                    ( J.TextDocumentSyncOptions
                        (Just True)
                        (Just syncKind)
                        (Just False)
                        (Just False)
                        (Just $ J.InR $ J.SaveOptions $ Just True)
                    )
              }
        }
 where
  -- Using SyncFull seems to handle the debounce for us.
  -- The alternative is to use SyncIncremental, but then then
  -- handler is called for each key-stroke.
  syncKind = J.TdSyncFull

diagnosticSourcePrefix :: Text
diagnosticSourcePrefix = "swarm-lsp"

debug :: MonadIO m => Text -> m ()
debug msg = liftIO $ Text.hPutStrLn stderr $ "[swarm-lsp] " <> msg

validateSwarmCode :: J.NormalizedUri -> J.TextDocumentVersion -> Text -> LspM () ()
validateSwarmCode doc version content = do
  -- debug $ "Validating: " <> from (show doc) <> " ( " <> content <> ")"

  -- FIXME: #1040 With this call to flushDiagnosticsBySource in place, the warnings
  -- in other buffers (editor tabs) end up getting cleared when switching between
  -- (focusing on) other buffers in VS Code.
  -- However, getting rid of this seems to break error highlighting.
  flushDiagnosticsBySource 0 (Just diagnosticSourcePrefix)

  let (parsingErrs, unusedVarWarnings) = case readTerm' content of
        Right Nothing -> ([], [])
        Right (Just term) -> (parsingErrors, unusedWarnings)
         where
          VU.Usage _ problems = VU.getUsage mempty term
          unusedWarnings = mapMaybe (VU.toErrPos content) problems

          parsingErrors = case processParsedTerm' mempty mempty term of
            Right _ -> []
            Left e -> pure $ showTypeErrorPos content e
        Left e -> (pure $ showErrorPos e, [])
  -- debug $ "-> " <> from (show err)

  publishDiags $
    map makeUnusedVarDiagnostic unusedVarWarnings

  -- NOTE: "publishDiags" keeps only one diagnostic at a
  -- time (the most recent) so we make sure the errors are
  -- issued last (after any warnings).
  -- Note that it does not achieve the desired effect to simply
  -- concatenate the two diagnostic lists into a single
  -- publishDiagnostics function call (regardless of the order
  -- of the lists).
  publishDiags $
    map makeParseErrorDiagnostic parsingErrs
 where
  publishDiags :: [J.Diagnostic] -> LspM () ()
  publishDiags = publishDiagnostics 1 doc version . partitionBySource

  makeUnusedVarDiagnostic :: (J.Range, Text) -> J.Diagnostic
  makeUnusedVarDiagnostic (range, msg) =
    J.Diagnostic
      range
      (Just J.DsWarning) -- severity
      Nothing -- code
      (Just diagnosticSourcePrefix) -- source
      msg
      (Just (J.List [J.DtUnnecessary])) -- tags
      Nothing -- related source code info
  makeParseErrorDiagnostic :: ((Int, Int), (Int, Int), Text) -> J.Diagnostic
  makeParseErrorDiagnostic ((startLine, startCol), (endLine, endCol), msg) =
    J.Diagnostic
      ( J.Range
          (J.Position (fromIntegral startLine) (fromIntegral startCol))
          (J.Position (fromIntegral endLine) (fromIntegral endCol))
      )
      (Just J.DsError) -- severity
      Nothing -- code
      (Just diagnosticSourcePrefix) -- source
      msg
      Nothing -- tags
      (Just (J.List []))

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler J.SInitialized $ \_not -> do
        debug "Initialized"
    , notificationHandler J.STextDocumentDidSave $ \msg -> do
        let doc = msg ^. J.params . J.textDocument . J.uri
            content = fromMaybe "?" $ msg ^. J.params . J.text
        validateSwarmCode (J.toNormalizedUri doc) Nothing content
    , notificationHandler J.STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. J.params . J.textDocument . J.uri
            content = msg ^. J.params . J.textDocument . J.text
        validateSwarmCode (J.toNormalizedUri doc) Nothing content
    , notificationHandler J.STextDocumentDidChange $ \msg -> do
        let doc = msg ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
        mdoc <- getVirtualFile doc
        case mdoc of
          Just vf@(VirtualFile _ version _rope) -> do
            validateSwarmCode doc (Just $ fromIntegral version) (virtualFileText vf)
          _ -> debug $ "No virtual file found for: " <> from (show msg)
    , requestHandler J.STextDocumentHover $ \req responder -> do
        let doc = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
            pos = req ^. J.params . J.position
        mdoc <- getVirtualFile doc
        let maybeHover = do
              vf <- mdoc
              (markdownText, maybeRange) <- H.showHoverInfo doc Nothing pos vf
              return $ Hover (J.HoverContents $ J.MarkupContent J.MkMarkdown markdownText) maybeRange
        responder $ Right maybeHover
    ]
