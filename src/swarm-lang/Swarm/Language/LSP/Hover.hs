{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.LSP.Hover (
  showHoverInfo,

  -- * Documentation rendering
  renderDoc,
  treeToMarkdown,

  -- * Explaining source position
  explain,
)
where

import Control.Carrier.Error.Either (run, runError)
import Control.Lens ((^.))
import Data.Graph
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as R
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.VFS
import Swarm.Failure (SystemFailure)
import Swarm.Language.LSP.Position
import Swarm.Language.Module (moduleTerm)
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Pipeline (processTermNoImports)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types
import Swarm.Pretty (PrettyPrec, prettyText)
import Swarm.Util qualified as U

showHoverInfo ::
  J.NormalizedUri ->
  J.Position ->
  VirtualFile ->
  Maybe (Text, Maybe J.Range)
showHoverInfo _ p vf@(VirtualFile _ _ myRope) =
  either (const Nothing) (fmap genHoverInfo) (readTerm' defaultParserConfig content)
 where
  content = virtualFileText vf
  absolutePos =
    R.charLength . fst $ R.charSplitAtPosition (lspToRopePosition p) myRope

  genHoverInfo stx =
    case fmap moduleTerm (run (runError @SystemFailure (processTermNoImports content stx Nothing))) of
      Right (Just pt) ->
        let found = narrowToPosition pt $ fromIntegral absolutePos
            finalPos = posToRange myRope (found ^. sLoc)
         in (,finalPos) . treeToMarkdown 0 $ explain found
      _ ->
        let found = narrowToPosition stx $ fromIntegral absolutePos
            finalPos = posToRange myRope (found ^. sLoc)
         in (,finalPos) . treeToMarkdown 0 $ explain found

renderDoc :: Int -> Text -> Text
renderDoc d t
  | d == 0 = t
  | otherwise = T.drop 2 . indent (max 0 (4 * (d - 1)) + 2) $ "* " <> t
 where
  indent x = T.unlines . map (T.replicate x " " <>) . T.lines

treeToMarkdown :: Int -> Tree Text -> Text
treeToMarkdown d (Node t children) =
  T.unlines $ renderDoc d t : map (treeToMarkdown $ d + 1) children

explain :: (PrettyPrec (Anchor (ImportPhaseFor phase)), Unresolvable (ImportPhaseFor phase)) => ExplainableType (SwarmType phase) => Syntax phase -> Tree Text
explain trm = case trm ^. sTerm of
  TUnit -> literal "The unit value."
  TConst c -> literal . constGenSig c $ briefDoc (constDoc $ constInfo c)
  TDir {} -> literal "A direction literal."
  TInt {} -> literal "An integer literal."
  TText {} -> literal "A text literal."
  TBool {} -> literal "A boolean literal."
  TVar v -> pure $ typeSignature v ty ""
  SRcd {} -> literal "A record literal."
  SProj {} -> literal "A record projection."
  STydef {} -> literal "A type synonym definition."
  TType {} -> literal "A type literal."
  SParens s -> explain s
  SImportIn {} -> literal "An import expression."
  -- type ascription
  SAnnotate lhs typeAnn ->
    Node
      (typeSignature "_" typeAnn "A type ascription for")
      [explain lhs]
  -- special forms (function application will show for `$`, but really should be rare)
  SApp {} -> explainFunction trm
  TRequire {} -> pure "Require a specific device to be equipped."
  TStock {} -> pure "Stock a certain number of an entity."
  SRequirements {} -> pure "Query the requirements of a term."
  -- definition or bindings
  SLet ls isRecursive var mTypeAnn _ _ rhs _b -> pure $ explainDefinition ls isRecursive var (rhs ^. sType) mTypeAnn
  SLam (Loc _s v) _mType _syn ->
    pure $
      typeSignature v ty $
        "A lambda expression binding the variable " <> U.bquote v <> "."
  SBind mv _ _ _ rhs _cmds ->
    pure $
      typeSignature (maybe "__rhs" locVal mv) (getInnerType $ rhs ^. sType) $
        "A monadic bind for commands" <> maybe "." (\(Loc _s v) -> ", that binds variable " <> U.bquote v <> ".") mv
  -- composite types
  SPair {} ->
    Node
      (typeSignature "_" ty "A tuple consisting of:")
      (explain <$> unTuple trm)
  SDelay {} ->
    pure . T.unlines $
      [ "Delay evaluation of a term, written `{...}`."
      , ""
      , "Swarm is an eager language, but in some cases (e.g. for `if` statements and recursive bindings) we need to delay evaluation."
      , ""
      , "The counterpart to `{...}` is `force`:"
      , "```"
      , "force {t} = t"
      , "```"
      ]
  -- internal syntax that should not actually show in hover
  TRef {} -> internal "A memory reference."
  TAntiInt {} -> internal "An antiquoted Haskell variable name of type Integer."
  TAntiText {} -> internal "An antiquoted Haskell variable name of type Text."
  TAntiSyn {} -> internal "An antiquoted Haskell variable name of type Syntax."
  TRobot {} -> internal "A robot reference."
  SSuspend {} -> internal "A suspension."
 where
  ty = trm ^. sType
  literal = pure . typeSignature (prettyText $ trm ^. sTerm) ty
  internal description = literal $ description <> "\n**These should never show up in surface syntax.**"
  constGenSig c =
    let ity = inferConst c
     in U.applyWhen (not $ ty `eq` ity) $ typeSignature (prettyText c) ity

-- | Helper function to explain function application.
--
-- Note that 'Force' is often inserted internally, so
-- if it shows up here we drop it.
explainFunction :: (PrettyPrec (Anchor (ImportPhaseFor phase)), Unresolvable (ImportPhaseFor phase)) => ExplainableType (SwarmType phase) => Syntax phase -> Tree Text
explainFunction s =
  case unfoldApps s of
    (Syntax _ (TConst Force) _ _ :| [innerT]) -> explain innerT
    (Syntax _ (TConst Force) _ _ :| f : params) -> explainF f params
    (f :| params) -> explainF f params
 where
  explainF f params =
    Node
      "Function application of:"
      [ explain f
      , Node
          "with parameters:"
          (map explain params)
      ]

explainDefinition :: (ExplainableType ty) => LetSyntax -> Bool -> LocVar -> ty -> Maybe RawPolytype -> Text
explainDefinition ls isRecursive (Loc _s var) ty maybeTypeAnnotation =
  typeSignature var ty $
    T.unwords
      [ "A"
      , (if isRecursive then "" else "non-") <> "recursive"
      , if ls == LSDef then "definition" else "let"
      , "expression"
      , if isNothing maybeTypeAnnotation then "without" else "with"
      , "a type annotation on the variable."
      ]

typeSignature :: (ExplainableType ty) => Var -> ty -> Text -> Text
typeSignature v typ body = T.unlines ["```", short, "```", body]
 where
  short = v <> ": " <> prettyType typ
