module Data.BoolExpr.Printer (
  -- * Printers
  boolExprPrinter,
  signedPrinter,
  disjPrinter,
  conjPrinter,
  cnfPrinter,
  dnfPrinter,
)
where

import Data.BoolExpr

-- | Printer
boolExprPrinter :: (a -> ShowS) -> BoolExpr a -> ShowS
boolExprPrinter f = go
 where
  go (BAnd a b) = paren $ go a . text " AND " . go b
  go (BOr a b) = paren $ go a . text " OR " . go b
  go (BNot a) = text "-" . paren (go a)
  go BTrue = text "TRUE" -- not in the parser
  go BFalse = text "FALSE" -- not in the parser
  go (BConst c) = signedPrinter f c

sep :: String -> String -> (a -> ShowS) -> [a] -> ShowS
sep empty _ _ [] = text empty
sep _ s f xs = foldr1 (\x y -> x . text s . y) (f <$> xs)

signedPrinter :: (a -> ShowS) -> Signed a -> ShowS
signedPrinter f (Positive c) = f c
signedPrinter f (Negative c) = text "-" . f c

disjPrinter :: (a -> ShowS) -> Disj a -> ShowS
disjPrinter f = sep "FALSE" " OR " (paren . f) . unDisj

conjPrinter :: (a -> ShowS) -> Conj a -> ShowS
conjPrinter f = sep "TRUE" " AND " (paren . f) . unConj

cnfPrinter :: (a -> ShowS) -> CNF a -> ShowS
cnfPrinter f = conjPrinter (disjPrinter (signedPrinter f)) . unCNF

dnfPrinter :: (a -> ShowS) -> DNF a -> ShowS
dnfPrinter f = disjPrinter (conjPrinter (signedPrinter f)) . unDNF

paren :: ShowS -> ShowS
paren = showParen True

text :: String -> ShowS
text = showString
