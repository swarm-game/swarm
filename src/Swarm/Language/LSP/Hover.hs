{-# LANGUAGE OverloadedStrings #-}

module Swarm.Language.LSP.Hover (
  showHoverInfo,

  -- * Documentation rendering
  DocLine (..),
  renderDoc,
  treeToMarkdown,

  -- * Finding source location
  narrowToPosition,

  -- * Explaining source position
  Length (..),
  explain,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Graph
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Utf16.Rope qualified as R
import Language.LSP.Types qualified as J
import Language.LSP.VFS
import Swarm.Language.Context as Ctx
import Swarm.Language.Parse (readTerm')
import Swarm.Language.Pipeline (ProcessedTerm (..), processParsedTerm)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types (Polytype)
import Swarm.Util qualified as U
import Swarm.Language.Module (TModule, Module (moduleCtx))

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
    Right (Just stx@(Syntax l t)) -> Just (treeToMarkdown 0 $ explain Long mMod term, finalPos)
     where
      mMod = case processParsedTerm stx of
        Left _e -> Nothing
        Right (ProcessedTerm modul _req _reqCtx) -> Just modul
      Syntax sloc term = narrowToPosition (Syntax l t) $ fromIntegral absolutePos
      finalPos = do
        (s, e) <- case sloc of
          SrcLoc s e -> Just (s, e)
          _ -> Nothing
        (startRope, _) <- R.splitAt (fromIntegral s) myRope
        (endRope, _) <- R.splitAt (fromIntegral e) myRope
        return $
          J.Range
            (ropeToLspPosition $ R.lengthAsPosition startRope)
            (ropeToLspPosition $ R.lengthAsPosition endRope)
 where
  content = virtualFileText vf
  absolutePos =
    maybe 0 (R.length . fst) $
      R.splitAtPosition (lspToRopePosition p) myRope

descend ::
  -- | position
  Int ->
  -- | next element to inspect
  Syntax ->
  Maybe Syntax
descend pos s1@(Syntax l1 _) = do
  guard $ withinBound pos l1
  return $ narrowToPosition s1 pos

-- | Find the most specific term for a given
-- position within the code.
--
-- In the case statement, all of the constructors
-- with nested Syntax files are given explicit
-- cases, whereas a catchall for non-recursive
-- constructors is placed at the end.  Note that
-- this is catch-all is "dangerous" if we ever
-- add more recursive terms to the language; the
-- compiler won't warn us about missing cases.
narrowToPosition ::
  -- | parent term
  Syntax ->
  -- | absolute offset within the file
  Int ->
  Syntax
narrowToPosition s0@(Syntax _ t) pos = fromMaybe s0 $ case t of
  SLam _ _ s -> d s
  SApp s1 s2 -> d s1 <|> d s2
  SLet _ _ _ s1 s2 -> d s1 <|> d s2
  SPair s1 s2 -> d s1 <|> d s2
  SDef _ _ _ s -> d s
  SBind _ s1 s2 -> d s1 <|> d s2
  SDelay _ s -> d s
  _ -> Nothing
 where
  d = descend pos

-- | Markdown line that captures tree depth
data DocLine a = DocLine
  { depth :: Int
  , txt :: a
  }

renderDoc :: DocLine Text -> Text
renderDoc (DocLine d t)
  | d == 0 = t
  | otherwise = T.drop 2 . indent (max 0 (4 * (d - 1)) + 2) $ "* " <> t
 where
  indent x = T.unlines . map (T.replicate x " " <>) . T.lines

instance Functor DocLine where
  fmap f (DocLine d x) = DocLine d $ f x

pureDoc :: a -> DocLine a
pureDoc = DocLine 0

treeToMarkdown :: Int -> Tree (DocLine Text) -> Text
treeToMarkdown d (Node (DocLine _n t) children) =
  T.unlines $ renderDoc (DocLine d t) : map (treeToMarkdown $ d + 1) children

-- | Top level explanations can use fancy markdown syntax, but list items
--   have to be more conservative and ideally shorter.
data Length = Brief | Long
  deriving (Eq, Show)

explain :: Length -> Maybe TModule -> Term -> Tree (DocLine Text)
explain len mMod trm = case trm of
  TUnit -> pure $ pureDoc "The unit value."
  TConst c ->
    pure . pureDoc $
      typeSignature
        len
        (prettyText c)
        (Just $ inferConst c)
        (briefDoc $ constDoc $ constInfo c)
  TDir {} -> pure $ pureDoc "A direction literal."
  TInt {} -> pure $ pureDoc "An integer literal."
  TAntiInt {} -> pure $ pureDoc "An antiquoted Haskell variable name of type Integer."
  TText {} -> pure $ pureDoc "A text literal."
  TAntiText {} -> pure $ pureDoc "An antiquoted Haskell variable name of type Text."
  TBool {} -> pure $ pureDoc "A Boolean literal."
  TRobot {} -> pure $ pureDoc "A robot reference.  These never show up in surface syntax, but are here so we can factor pretty-printing for Values through pretty-printing for Terms."
  TRef {} -> pure $ pureDoc "A memory reference.  These likewise never show up in surface syntax but are here to facilitate pretty-printing."
  TRequireDevice {} -> pure $ pureDoc "Require a specific device to be equipped."
  TRequire {} -> pure $ pureDoc "Require a certain number of an entity."
  TVar v -> pure $ pureDoc $ "`" <> v <> ": " <> maybe "?" prettyText (Ctx.lookup v . moduleCtx =<< mMod) <> "`"
  SLam (LV _s v) _mType _syn -> pure . pureDoc $ "A lambda expression binding the variable " <> U.bquote v <> "."
  SApp _ _ -> explainFunction len mMod trm
  SLet isRecursive var maybeTypeAnnotation _r _b -> pure $ explainDefinition len False mMod isRecursive var maybeTypeAnnotation
  SDef isRecursive var maybeTypeAnnotation _body -> pure $ explainDefinition len True mMod isRecursive var maybeTypeAnnotation
  SPair {} -> pure $ pureDoc "A pair."
  SBind {} -> pure $ pureDoc "A monadic bind for commands, of the form `c1 ; c2` or `x <- c1; c2`."
  SDelay {} -> pure $ pureDoc "Delay evaluation of a term, written `{...}`.  Swarm is an eager language, but in some cases (e.g. for `if` statements and recursive bindings) we need to delay evaluation.  The counterpart to `{...}` is `force`, where `force {t} = t`. Note that 'Force' is just a constant, whereas 'SDelay' has to be a special syntactic form so its argument can get special treatment during evaluation."

-- | Helper function to explain function application.
--
-- Note that 'Force' is often inserted internally, so
-- if it shows up here we drop it.
explainFunction :: Length -> Maybe TModule -> Term -> Tree (DocLine Text)
explainFunction len mMod t =
  case unfoldApps t of
    (TConst Force :| [innerT]) -> explain len mMod innerT
    (TConst Force :| f : params) -> explainF f params
    (f :| params) -> explainF f params
 where
  explainF f params =
    Node
      (pureDoc "Function application of:")
      [ explain Brief mMod f
      , Node
          (pureDoc "with parameters:")
          (map (explain Brief mMod) params)
      ]

explainDefinition :: Length -> Bool -> Maybe TModule -> Bool -> LocVar -> Maybe Polytype -> DocLine Text
explainDefinition len isDef mMod isRecursive (LV _s var) maybeTypeAnnotation =
  pureDoc $
    typeSignature len var (maybeTypeAnnotation <|> mTyp) $
      T.unwords
        [ "A"
        , (if isRecursive then "" else "non-") <> "recursive"
        , if isDef then "definition" else "let"
        , "expression"
        , if null maybeTypeAnnotation then "without" else "with"
        , "a type annotation on the variable."
        ]
 where
  mTyp = Ctx.lookup var . moduleCtx =<< mMod

typeSignature :: Length -> Var -> Maybe Polytype -> Text -> Text
typeSignature len v typ body =
  T.unlines $
    case len of
      Brief -> ["`" <> short <> "`", body]
      Long -> ["```", short, "```", body]
 where
  ptyp = maybe "?" prettyText typ
  short = v <> ": " <> ptyp
