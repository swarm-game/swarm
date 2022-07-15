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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import System.IO (stderr)
import Witch

import Language.LSP.Diagnostics
import Language.LSP.Server
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import Language.LSP.VFS

import Swarm.Language.Parse
import Swarm.Language.Pipeline

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
              { -- set sync options to get DidSave event
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

debug :: MonadIO m => Text -> m ()
debug msg = liftIO $ Text.hPutStrLn stderr $ "[swarm-lsp] " <> msg

validateSwarmCode :: J.NormalizedUri -> J.TextDocumentVersion -> Text -> LspM () ()
validateSwarmCode doc version content = do
  -- debug $ "Validating: " <> from (show doc) <> " ( " <> content <> ")"
  flushDiagnosticsBySource 0 (Just "swarm-lsp")
  let err = case readTerm' content of
        Right Nothing -> Nothing
        Right (Just term) -> case processParsedTerm' mempty mempty term of
          Right _ -> Nothing
          Left e -> Just $ showTypeErrorPos content e
        Left e -> Just $ showErrorPos e
  -- debug $ "-> " <> from (show err)
  case err of
    Nothing -> pure ()
    Just e -> sendDiagnostic e
 where
  sendDiagnostic :: ((Int, Int), (Int, Int), Text) -> LspM () ()
  sendDiagnostic ((startLine, startCol), (endLine, endCol), msg) = do
    let diags =
          [ J.Diagnostic
              ( J.Range
                  (J.Position (fromIntegral startLine) (fromIntegral startCol))
                  (J.Position (fromIntegral endLine) (fromIntegral endCol))
              )
              (Just J.DsWarning) -- severity
              Nothing -- code
              (Just "swarm-lsp") -- source
              msg
              Nothing -- tags
              (Just (J.List []))
          ]
    publishDiagnostics 1 doc version (partitionBySource diags)

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
    ]
