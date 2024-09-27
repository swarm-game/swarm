{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Common pretty-printing infrastructure for the Swarm project.
module Swarm.Pretty (
  -- * The 'PrettyPrec' class
  PrettyPrec (..),

  -- * Running pretty-printers
  ppr,
  prettyText,
  prettyTextWidth,
  prettyTextLine,
  prettyString,
  docToText,
  docToTextWidth,
  docToString,

  -- * Pretty-printing utilities
  pparens,
  pparens',
  encloseWithIndent,
  bquote,
  prettyShowLow,
  reportBug,
  Prec (..),
  BulletList (..),
  prettyBinding,
  prettyEquality,
  Wildcard (..),
) where

import Control.Monad.Free
import Data.Fix (Fix, unFix)
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.String qualified as RS
import Prettyprinter.Render.Text qualified as RT
import Swarm.Util (showLowT)

------------------------------------------------------------
-- PrettyPrec class + utilities

-- | Type class for things that can be pretty-printed, given a
--   precedence level of their context.
class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann -- can replace with custom ann type later if desired

instance PrettyPrec Text where
  prettyPrec _ = pretty

instance (PrettyPrec (t (Fix t))) => PrettyPrec (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance (PrettyPrec (t (Free t v)), PrettyPrec v) => PrettyPrec (Free t v) where
  prettyPrec p (Free t) = prettyPrec p t
  prettyPrec p (Pure v) = prettyPrec p v

-- | Pretty-print a thing, with a context precedence level of zero.
ppr :: (PrettyPrec a) => a -> Doc ann
ppr = prettyPrec 0

-- | Render a pretty-printed document as @Text@.
docToText :: Doc a -> Text
docToText = RT.renderStrict . layoutPretty defaultLayoutOptions

-- | Render a pretty-printed document as @Text@.
--   This function consumes number of allowed characters in a
--   line before introducing a line break. In other words, it
--   expects the space of the layouter to be supplied.
docToTextWidth :: Doc a -> Int -> Text
docToTextWidth doc layoutWidth =
  RT.renderStrict $ layoutPretty (LayoutOptions (AvailablePerLine layoutWidth 1.0)) doc

-- | Pretty-print something and render it as @Text@.
prettyText :: (PrettyPrec a) => a -> Text
prettyText = docToText . ppr

-- | Pretty-print something and render it as @Text@.
--   This is different than @prettyText@ in the sense that it also
--   consumes number of allowed characters in a line before introducing
--   a line break.
prettyTextWidth :: (PrettyPrec a) => a -> Int -> Text
prettyTextWidth = docToTextWidth . ppr

-- | Pretty-print something and render it as (preferably) one line @Text@.
prettyTextLine :: (PrettyPrec a) => a -> Text
prettyTextLine = RT.renderStrict . layoutPretty (LayoutOptions Unbounded) . group . ppr

-- | Render a pretty-printed document as a @String@.
docToString :: Doc a -> String
docToString = RS.renderString . layoutPretty defaultLayoutOptions

-- | Pretty-print something and render it as a @String@.
prettyString :: (PrettyPrec a) => a -> String
prettyString = docToString . ppr

-- | Optionally surround a document with parentheses depending on the
--   @Bool@ argument and if it does not fit on line, indent the lines,
--   with the parens on separate lines.
pparens :: Bool -> Doc ann -> Doc ann
pparens True = group . encloseWithIndent 2 lparen rparen
pparens False = id

-- | Same as pparens but does not indent the lines. Only encloses
--   the document with parantheses.
pparens' :: Bool -> Doc ann -> Doc ann
pparens' True = group . enclose lparen rparen
pparens' False = id

encloseWithIndent :: Int -> Doc ann -> Doc ann -> Doc ann -> Doc ann
encloseWithIndent i l r = nest i . enclose (l <> line') (nest (-2) $ line' <> r)

-- | Surround a document with backticks.
bquote :: Doc ann -> Doc ann
bquote = group . enclose "`" "`"

-- | Turn a 'Show' instance into a @Doc@, lowercasing it in the
--   process.
prettyShowLow :: Show a => a -> Doc ann
prettyShowLow = pretty . showLowT

-- | An invitation to report an error as a bug.
reportBug :: Doc ann
reportBug = "This should never happen; please report this as a bug: https://github.com/swarm-game/swarm/issues/new"

--------------------------------------------------
-- Bullet lists

data Prec a = Prec Int a

data BulletList i = BulletList
  { bulletListHeader :: forall a. Doc a
  , bulletListItems :: [i]
  }

instance (PrettyPrec i) => PrettyPrec (BulletList i) where
  prettyPrec _ (BulletList hdr items) =
    nest 2 . vcat $ hdr : map (("-" <+>) . ppr) items

--------------------------------------------------
-- Term- and type-printing utilities: bindings, equalities, wildcards,
-- etc.

prettyBinding :: (Pretty a, PrettyPrec b) => (a, b) -> Doc ann
prettyBinding (x, ty) = pretty x <> ":" <+> ppr ty

prettyEquality :: (Pretty a, PrettyPrec b) => (a, Maybe b) -> Doc ann
prettyEquality (x, Nothing) = pretty x
prettyEquality (x, Just t) = pretty x <+> "=" <+> ppr t

-- | We can use the 'Wildcard' value to replace unification variables
--   when we don't care about them, e.g. to print out the shape of a
--   type like @(_ -> _) * _@
data Wildcard = Wildcard
  deriving (Eq, Ord, Show)

instance PrettyPrec Wildcard where
  prettyPrec _ _ = "_"
