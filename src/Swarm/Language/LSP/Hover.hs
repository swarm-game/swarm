{-# LANGUAGE OverloadedStrings #-}

module Swarm.Language.LSP.Hover where

import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Syntax
import Language.LSP.Types qualified as J
import Data.Rope.UTF16 qualified as R
import Language.LSP.VFS
import Swarm.Language.Parse ( readTerm' )

withinBound :: Int -> SrcLoc -> Bool
withinBound pos (SrcLoc start end) = pos >= start && pos < end
withinBound _ NoLoc = False

showHoverInfo
  :: J.NormalizedUri
  -> J.TextDocumentVersion
  -> J.Position
  -> VirtualFile
  -> Maybe Text
showHoverInfo _ _ (J.Position myLine myCol) vf@(VirtualFile _ _ myRope) =
  astSize
  where
    content = virtualFileText vf
    absolutePos = R.rowColumnCodeUnits (R.RowColumn (fromIntegral myLine) (fromIntegral myCol)) myRope
    astSize = case readTerm' content of
      Right Nothing -> Nothing
      Right (Just (Syntax _ term)) -> Just $ explain $ digAt term absolutePos
      Left _ -> Nothing

descend
  :: Term
     -- ^ default
  -> Int
     -- ^ position
  -> Syntax
     -- ^ next element to inspect
  -> Term
descend t pos (Syntax l1 t1) = if withinBound pos l1
  then digAt t1 pos
  else t

descend2
  :: Term
     -- ^ default
  -> Int
     -- ^ position
  -> Syntax
     -- ^ next element to inspect
  -> Syntax
     -- ^ alternate element to inspect
  -> Term
descend2 t pos (Syntax l1 t1) s2 =
  if withinBound pos l1
  then digAt t1 pos
  else descend t pos s2

digAt :: Term -> Int -> Term
digAt t pos = case t of
  SLam _ _ s -> descend t pos s
  SApp s1 s2 -> descend2 t pos s1 s2
  SLet _ _ _ s1 s2 -> descend2 t pos s1 s2
  SPair s1 s2 -> descend2 t pos s1 s2
  SDef _ _ _ s -> descend t pos s
  SBind _ s1 s2 -> descend2 t pos s1 s2
  SDelay _ s -> descend t pos s
  x -> x

explain :: Term -> Text
explain = \case
    TUnit -> "The unit value."
    TConst c -> briefDoc $ constDoc $ constInfo c
    TDir {} -> "A direction literal."
    TInt {} -> "An integer literal."
    TAntiInt {} -> "An antiquoted Haskell variable name of type Integer."
    TText {} -> "A text literal."
    TAntiText {} -> "An antiquoted Haskell variable name of type Text."
    TBool {} -> "A Boolean literal."
    TRobot {} -> "A robot reference.  These never show up in surface syntax, but are here so we can factor pretty-printing for Values through pretty-printing for Terms."
    TRef {} -> "A memory reference.  These likewise never show up in surface syntax but are here to facilitate pretty-printing."
    TRequireDevice {} -> "Require a specific device to be installed."
    TRequire {} -> "Require a certain number of an entity."
    TVar {} -> "A variable."
    SLam {} -> "A lambda expression, with or without a type annotation on the binder."
    SApp {} -> "Function application."
    SLet {} -> "A (recursive) let expression, with or without a type annotation on the variable. The @Bool@ indicates whether it is known to be recursive."
    SPair {} -> "A pair."
    SDef {} -> "A (recursive) definition command, which binds a variable to a value in subsequent commands. The @Bool@ indicates whether the definition is known to be recursive."
    SBind {} -> "A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@."
    SDelay {} -> "Delay evaluation of a term, written @{...}@.  Swarm is an eager language, but in some cases (e.g. for @if@ statements and recursive bindings) we need to delay evaluation.  The counterpart to @{...}@ is @force@, where @force {t} = t@. Note that 'Force' is just a constant, whereas 'SDelay' has to be a special syntactic form so its argument can get special treatment during evaluation."
