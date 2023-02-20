{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Failure.Render where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (prettyPrintParseException)
import Swarm.Game.Failure

tShowLow :: Show a => a -> Text
tShowLow = T.pack . map toLower . show

tShow :: Show a => a -> Text
tShow = T.pack . show

prettyPathLoadingFailure :: PathLoadFailure -> Text
prettyPathLoadingFailure (PathLoadFailure p x) =
  T.unwords
    [ "For path"
    , T.pack p <> ","
    , prettyLoadingFailure x
    ]

prettyLoadingFailure :: LoadingFailure -> Text
prettyLoadingFailure = \case
  DoesNotExist e -> "The " <> tShowLow e <> " is missing!"
  EntryNot e -> "The entry is not a " <> tShowLow e <> "!"
  CanNotParse p -> "Parse failure:\n" <> T.pack (indent 8 $ prettyPrintParseException p)
  CustomMessage m -> m
 where
  indent n = unlines . map (replicate n ' ' ++) . lines

prettyFailure :: SystemFailure -> Text
prettyFailure = \case
  AssetNotLoaded a (PathLoadFailure fp l) ->
    T.unwords ["Failed to acquire", tShowLow a, tShow fp] <> ": " <> prettyLoadingFailure l
