
-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Validates an uploaded scenario
module Swarm.Web.Tournament.Validate.Upload where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Either.Extra (maybeToEither)
import Data.List.NonEmpty qualified as NE
import Servant.Multipart
import Swarm.Game.State
import Swarm.Web.Tournament.Database.Query
import Swarm.Web.Tournament.Type
import Swarm.Web.Tournament.Validate.FailureMode

data PersistenceArgs m a
  = PersistenceArgs
      UserAlias
      (MultipartData Mem)
      (ScenarioPersistence m a)

obtainFileUpload ::
  MultipartData Mem ->
  ExceptT GenericUploadFailure IO FileUpload
obtainFileUpload multipartData =
  withExceptT GenericUploadFailure $ do
    nonemptyFiles <-
      except $
        maybeToEither NoFileSupplied maybeNonemptyFiles

    let suppliedCount = NE.length nonemptyFiles
    when (suppliedCount > 1) . except . Left $ MultipleFiles suppliedCount

    let file = NE.head nonemptyFiles
        content = fdPayload file
        theSha1Hash = Sha1 $ showDigest $ sha1 content

    return $ FileUpload content $ FileMetadata (fdFileName file) theSha1Hash
 where
  maybeNonemptyFiles = NE.nonEmpty $ files multipartData

withFileCache ::
  PersistenceArgs IO a ->
  (GenericUploadFailure -> e) ->
  (FileUpload -> ExceptT e IO (AssociatedSolutionCharacterization, a)) ->
  ExceptT e IO (FileMetadata, AssociatedSolutionCharacterization)
withFileCache (PersistenceArgs userAlias multipartData persistenceFunctions) errorWrapper cacheStoreFunction = do
  file <- withExceptT errorWrapper $ obtainFileUpload multipartData
  maybePreexisting <-
    liftIO
      . lookupCache persistenceFunctions
      . fileHash
      $ fileMetadata file
  solnMetrics <- maybe (doStore file) return maybePreexisting
  return (fileMetadata file, solnMetrics)
 where
  doStore file = do
    (result, a) <- cacheStoreFunction file

    liftIO
      . void
      . storeCache persistenceFunctions
      $ CharacterizationResponse
        (UserAttributedUpload userAlias file)
        result
        a

    return result
