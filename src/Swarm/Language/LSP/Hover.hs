{-# LANGUAGE OverloadedStrings #-}

module Swarm.Language.LSP.Hover (
  showHoverInfo,

  -- * Documentation rendering
  renderDoc,
  treeToMarkdown,

  -- * Finding source location
  narrowToPosition,

  -- * Explaining source position
  explain,
) where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (guard, void)
import Data.Graph
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Utf16.Rope qualified as R
import Language.LSP.Types qualified as J
import Language.LSP.VFS
import Swarm.Language.Context as Ctx
import Swarm.Language.Module (Module (..))
import Swarm.Language.Parse (readTerm', unTuple)
import Swarm.Language.Pipeline (ProcessedTerm (..), processParsedTerm)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types
import Swarm.Util qualified as U

withinBound :: Int -> SrcLoc -> Bool
withinBound pos (SrcLoc s e) = pos >= s && pos < e
withinBound _ NoLoc = False

ropeToLspPosition :: R.Position -> J.Position
ropeToLspPosition (R.Position l c) =
  J.Position (fromIntegral l) (fromIntegral c)

lspToRopePosition :: J.Position -> R.Position
lspToRopePosition (J.Position myLine myCol) =
  R.Position (fromIntegral myLine) (fromIntegral myCol)

showHoverInfo ::
  J.NormalizedUri ->
  J.TextDocumentVersion ->
  J.Position ->
  VirtualFile ->
  Maybe (Text, Maybe J.Range)
showHoverInfo _ _ p vf@(VirtualFile _ _ myRope) =
  case readTerm' content of
    Left _ -> Nothing
    Right Nothing -> Nothing
    Right (Just stx) -> Just $ case processParsedTerm stx of
      Left _e ->
        let found@(Syntax foundSloc _) = narrowToPosition stx $ fromIntegral absolutePos
            finalPos = posToRange myRope foundSloc
         in (,finalPos) . treeToMarkdown 0 $ explain found
      Right (ProcessedTerm modul _req _reqCtx) ->
        let found@(Syntax' foundSloc _ _) = narrowToPosition (moduleAST modul) $ fromIntegral absolutePos
            finalPos = posToRange myRope foundSloc
         in (,finalPos) . treeToMarkdown 0 $ explain found
 where
  content = virtualFileText vf
  absolutePos =
    maybe 0 (R.length . fst) $
      R.splitAtPosition (lspToRopePosition p) myRope

posToRange :: R.Rope -> SrcLoc -> Maybe J.Range
posToRange myRope foundSloc = do
  (s, e) <- case foundSloc of
    SrcLoc s e -> Just (s, e)
    _ -> Nothing
  (startRope, _) <- R.splitAt (fromIntegral s) myRope
  (endRope, _) <- R.splitAt (fromIntegral e) myRope
  return $
    J.Range
      (ropeToLspPosition $ R.lengthAsPosition startRope)
      (ropeToLspPosition $ R.lengthAsPosition endRope)

descend ::
  ExplainableType ty =>
  -- | position
  Int ->
  -- | next element to inspect
  Syntax' ty ->
  Maybe (Syntax' ty)
descend pos s1@(Syntax' l1 _ _) = do
  guard $ withinBound pos l1
  return $ narrowToPosition s1 pos

-- | Find the most specific term for a given
-- position within the code.
narrowToPosition ::
  ExplainableType ty =>
  -- | parent term
  Syntax' ty ->
  -- | absolute offset within the file
  Int ->
  Syntax' ty
narrowToPosition s0@(Syntax' _ t ty) pos = fromMaybe s0 $ case t of
  SLam lv _ s -> d (locVarToSyntax' lv $ getInnerType ty) <|> d s
  SApp s1 s2 -> d s1 <|> d s2
  SLet _ lv _ s1@(Syntax' _ _ lty) s2 -> d (locVarToSyntax' lv lty) <|> d s1 <|> d s2
  SDef _ lv _ s@(Syntax' _ _ lty) -> d (locVarToSyntax' lv lty) <|> d s
  SBind mlv s1@(Syntax' _ _ lty) s2 -> (mlv >>= d . flip locVarToSyntax' (getInnerType lty)) <|> d s1 <|> d s2
  SPair s1 s2 -> d s1 <|> d s2
  SDelay _ s -> d s
  -- atoms - return their position and end recursion
  TUnit -> Nothing
  TConst {} -> Nothing
  TDir {} -> Nothing
  TInt {} -> Nothing
  TText {} -> Nothing
  TBool {} -> Nothing
  TVar {} -> Nothing
  TRequire {} -> Nothing
  TRequireDevice {} -> Nothing
  -- these should not show up in surface language
  TRef {} -> Nothing
  TRobot {} -> Nothing
  TAntiInt {} -> Nothing
  TAntiText {} -> Nothing
 where
  d = descend pos

renderDoc :: Int -> Text -> Text
renderDoc d t
  | d == 0 = t
  | otherwise = T.drop 2 . indent (max 0 (4 * (d - 1)) + 2) $ "* " <> t
 where
  indent x = T.unlines . map (T.replicate x " " <>) . T.lines

treeToMarkdown :: Int -> Tree Text -> Text
treeToMarkdown d (Node t children) =
  T.unlines $ renderDoc d t : map (treeToMarkdown $ d + 1) children

class Show t => ExplainableType t where
  -- | Pretty print the type.
  prettyType :: t -> Text

  -- | Strip the type of its outermost layer.
  --
  -- This allows us to strip lambda or command type
  -- and get the type of the bound variable.
  getInnerType :: t -> t

  -- | Check if this type is same as the given 'Polytype'.
  --
  -- We use it to not print same type twice (e.g. inferred and generic).
  eq :: t -> Polytype -> Bool

instance ExplainableType () where
  prettyType = const "?"
  getInnerType = id
  eq _ _ = False

instance ExplainableType Polytype where
  prettyType = prettyText
  getInnerType = fmap $ \case
    (l :->: _r) -> l
    (TyCmd t) -> t
    t -> t
  eq = (==)

explain :: ExplainableType ty => Syntax' ty -> Tree Text
explain trm = case trm ^. sTerm of
  TUnit -> literal "The unit value."
  TConst c -> literal . constGenSig c $ briefDoc (constDoc $ constInfo c)
  TDir {} -> literal "A direction literal."
  TInt {} -> literal "An integer literal."
  TText {} -> literal "A text literal."
  TBool {} -> literal "A boolean literal."
  TVar v -> pure $ typeSignature v ty ""
  -- special forms (function application will show for `$`, but really should be rare)
  SApp {} -> explainFunction trm
  TRequireDevice {} -> pure "Require a specific device to be equipped."
  TRequire {} -> pure "Require a certain number of an entity."
  -- definition or bindings
  SLet isRecursive var mTypeAnn rhs _b -> pure $ explainDefinition False isRecursive var (rhs ^. sType) mTypeAnn
  SDef isRecursive var mTypeAnn rhs -> pure $ explainDefinition True isRecursive var (rhs ^. sType) mTypeAnn
  SLam (LV _s v) _mType _syn ->
    pure $
      typeSignature v ty $
        "A lambda expression binding the variable " <> U.bquote v <> "."
  SBind mv rhs _cmds ->
    pure $
      typeSignature (maybe "__rhs" lvVar mv) (getInnerType $ rhs ^. sType) $
        "A monadic bind for commands" <> maybe "." (\(LV _s v) -> ", that binds variable " <> U.bquote v <> ".") mv
  -- composite types
  SPair {} ->
    Node
      (typeSignature "_" ty "A tuple consisting of:")
      (explain <$> unTuple trm)
  SDelay {} ->
    pure . T.unlines $
      [ "Delay evaluation of a term, written `{...}`."
      , "Swarm is an eager language, but in some cases (e.g. for `if` statements and recursive bindings) we need to delay evaluation."
      , "The counterpart to `{...}` is `force`, where `force {t} = t`."
      ]
  -- internal syntax that should not actually show in hover
  TRef {} -> internal "A memory reference."
  TAntiInt {} -> internal "An antiquoted Haskell variable name of type Integer."
  TAntiText {} -> internal "An antiquoted Haskell variable name of type Text."
  TRobot {} -> internal "A robot reference."
 where
  ty = trm ^. sType
  literal = pure . typeSignature (prettyText . void $ trm ^. sTerm) ty
  internal description = literal $ description <> "\n**These should never show up in surface syntax.**"
  constGenSig c =
    let ity = inferConst c
     in if ty `eq` ity then id else typeSignature (prettyText c) ity

-- | Helper function to explain function application.
--
-- Note that 'Force' is often inserted internally, so
-- if it shows up here we drop it.
explainFunction :: ExplainableType ty => Syntax' ty -> Tree Text
explainFunction s =
  case unfoldApps s of
    (Syntax' _ (TConst Force) _ :| [innerT]) -> explain innerT
    (Syntax' _ (TConst Force) _ :| f : params) -> explainF f params
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

explainDefinition :: ExplainableType ty => Bool -> Bool -> LocVar -> ty -> Maybe Polytype -> Text
explainDefinition isDef isRecursive (LV _s var) ty maybeTypeAnnotation =
  typeSignature var ty $
    T.unwords
      [ "A"
      , (if isRecursive then "" else "non-") <> "recursive"
      , if isDef then "definition" else "let"
      , "expression"
      , if null maybeTypeAnnotation then "without" else "with"
      , "a type annotation on the variable."
      ]

typeSignature :: ExplainableType ty => Var -> ty -> Text -> Text
typeSignature v typ body = T.unlines ["```", short, "```", body]
 where
  short = v <> ": " <> prettyType typ
