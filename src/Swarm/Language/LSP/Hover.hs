{-# LANGUAGE OverloadedStrings #-}

module Swarm.Language.LSP.Hover where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Graph
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Utf16.Rope qualified as R
import Language.LSP.Types qualified as J
import Language.LSP.VFS
import Swarm.Language.Parse (readTerm')
import Swarm.Language.Syntax
import Swarm.Util qualified as U
import Swarm.Language.Pretty ( prettyText, prettyString )
import Data.Tree (drawTree)

withinBound :: Int -> SrcLoc -> Bool
withinBound pos (SrcLoc s e) = pos >= s && pos < e
withinBound _ NoLoc = False

ropeToLspPosition :: R.Position -> J.Position
ropeToLspPosition (R.Position l c) =
  J.Position (fromIntegral l) (fromIntegral c)

lspToRopePosition :: J.Position -> R.Position
lspToRopePosition (J.Position myLine myCol) =
  R.Position (fromIntegral myLine) (fromIntegral myCol)

prettifySyntax :: Syntax -> String
prettifySyntax (Syntax _ t) = prettyString t

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
    Right (Just stx) -> Just (t, finalPos)
     where
      t = T.intercalate "\n\n" [
          prettyText term
        , U.bquote $ U.bquote $ U.bquote $ T.pack (drawTree $ prettifySyntax <$> syntaxAsTree mySyntax)
        , treeToMarkdown 0 (explain term)
        ]

      mySyntax@(Syntax sloc term) = narrowToPosition stx $ fromIntegral absolutePos
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

-- | Useful for "printTree" with "prettyText"
syntaxAsTree :: Syntax -> Tree Syntax
syntaxAsTree s0@(Syntax _pos t) = case t of
  TVar _ -> pure s0
  SLam _ _ s -> Node s0 [syntaxAsTree s]
  SApp s1 s2 -> Node s0 [syntaxAsTree s1, syntaxAsTree s2]
  SLet _ _ _ s1 s2 -> Node s0 [syntaxAsTree s1, syntaxAsTree s2]
  SPair s1 s2 -> Node s0 [syntaxAsTree s1, syntaxAsTree s2]
  SDef _ _v _ s -> Node s0 [syntaxAsTree s]
  SBind _v s1 s2 -> Node s0 [syntaxAsTree s1, syntaxAsTree s2]
  SDelay _ s -> Node s0 [syntaxAsTree s]
  _ -> pure s0

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
  | d == 1 = "* " <> t
  | otherwise = indent (d - 1) <> "* " <> t
 where
  indent x = T.replicate (4 * x) " "

instance Functor DocLine where
  fmap f (DocLine d x) = DocLine d $ f x

pureDoc :: a -> DocLine a
pureDoc = DocLine 0

treeToMarkdown :: Int -> Tree (DocLine Text) -> Text
treeToMarkdown d (Node (DocLine _n t) children) =
  T.unlines $ renderDoc (DocLine d t) : map (treeToMarkdown $ d + 1) children

explain :: Term -> Tree (DocLine Text)
explain = \case
  TUnit -> pure $ pureDoc "The unit value."
  TConst c -> pure $ pureDoc $ briefDoc $ constDoc $ constInfo c
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
  TVar x -> pure $ pureDoc $ "var: " <> U.bquote x
  SLam {} -> pure $ pureDoc "A lambda expression, with or without a type annotation on the binder."
  SApp (Syntax _ t1) (Syntax _ t2) ->
    Node
      (pureDoc "Function application of:")
      [explain t1, explain t2]
  SLet isRecursive _ maybeTypeAnnotation _ _ ->
    pure $
      pureDoc $
        T.unwords
          [ "A"
          , (if isRecursive then "" else "non-") <> "recursive"
          , "let expression"
          , if null maybeTypeAnnotation then "without" else "with"
          , "a type annotation on the variable."
          ]
  SPair {} -> pure $ pureDoc "A pair."
  SDef {} -> pure $ pureDoc "A (recursive) definition command, which binds a variable to a value in subsequent commands."
  SBind {} -> pure $ pureDoc "A monadic bind for commands, of the form `c1 ; c2` or `x <- c1; c2`."
  SDelay {} -> pure $ pureDoc "Delay evaluation of a term, written `{...}`.  Swarm is an eager language, but in some cases (e.g. for `if` statements and recursive bindings) we need to delay evaluation.  The counterpart to `{...}` is `force`, where `force {t} = t`. Note that 'Force' is just a constant, whereas 'SDelay' has to be a special syntactic form so its argument can get special treatment during evaluation."
