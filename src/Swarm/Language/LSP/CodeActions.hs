{-# LANGUAGE OverloadedStrings #-}

module Swarm.Language.LSP.CodeActions where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Utf16.Rope qualified as R
import Language.LSP.Types qualified as J
import Swarm.Language.Module (Module (..))
import Swarm.Language.Parse (readTerm')
import Swarm.Language.Pipeline (ProcessedTerm (..), processParsedTerm')
import Swarm.Language.Syntax
    ( Syntax'(Syntax'),
      Term'(SDelay, SLam, SDef, SApp, SLet, SPair, SBind) )
import Swarm.Language.Types
import Swarm.Language.LSP.Util
import Swarm.Language.LSP.Hover (prettyType)


data TypeSuggestion = TypeSuggestion Polytype (Syntax' Polytype)


suggestTypes :: R.Rope -> Text -> [J.TextEdit]
suggestTypes rope content =
  suggestions
  where
  suggestions = case readTerm' content of
    Right (Just term) -> case processParsedTerm' mempty mempty term of
      Right (ProcessedTerm (Module stx _) _ _) -> mapMaybe makeEdit $ getSites stx
      Left _ -> []
    _ -> []

  makeEdit :: TypeSuggestion -> Maybe J.TextEdit
  makeEdit (TypeSuggestion ty (Syntax' pos _term _ty)) = do
    J.Range (J.Position sLine sChar) (J.Position eLine eChar) <- posToRange rope pos
    let newRange = J.Range (J.Position sLine (sChar + 4)) (J.Position sLine (sChar + 5))
    return $ J.TextEdit newRange typeText
    where
      typeText = " : " <> prettyType ty

getSites :: Syntax' Polytype -> [TypeSuggestion]
getSites s0@(Syntax' _pos term _ty) = case term of
  -- SLam _v mayType s@(Syntax' _pos _term ty) -> [TypeSuggestion ty s0 | null mayType] <> getSites s
  -- TODO
  SLam _v mayType s@(Syntax' _pos _term ty) -> getSites s
  SDef _ _v mayType s@(Syntax' _pos _term ty) -> [TypeSuggestion ty s0 | null mayType] <> getSites s
  SApp s1 s2 -> getSites s1 <> getSites s2
  SLet _ _v _ s1 s2 -> getSites s1 <> getSites s2
  SPair s1 s2 -> getSites s1 <> getSites s2
  SBind _v s1 s2 -> getSites s1 <> getSites s2
  SDelay _ s -> getSites s
  _ -> mempty
