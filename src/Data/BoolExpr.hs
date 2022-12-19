{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

--------------------------------------------------------------------

--------------------------------------------------------------------

-- |
-- Module    : Data.BoolExpr
-- Copyright : (c) Nicolas Pouillard 2008,2009
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
-- Boolean expressions and various representations.
module Data.BoolExpr (
  -- * A boolean class
  Boolean (..),

  -- * Generic functions derived from Boolean
  bAnd,
  bAll,
  bOr,
  bAny,

  -- * Boolean trees
  BoolExpr (..),
  reduceBoolExpr,
  evalBoolExpr,

  -- * Boolean evaluation semantic
  Eval (..),
  runEvalId,

  -- * Signed constants
  Signed (..),
  negateSigned,
  evalSigned,
  reduceSigned,
  constants,
  negateConstant,

  -- * Conjunctive Normal Form
  CNF (..),
  Conj (..),
  fromCNF,
  boolTreeToCNF,
  reduceCNF,

  -- * Disjunctive Normal Form
  Disj (..),
  DNF (..),
  fromDNF,
  boolTreeToDNF,
  reduceDNF,

  -- * Other transformations
  dualize,
  fromBoolExpr,
  pushNotInwards,
)
where

-- import Test.QuickCheck hiding (Positive)
-- import Control.Applicative
import Control.Monad (ap)
import Data.Traversable

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics (Generic)

-- | Signed values are either positive or negative.
data Signed a = Positive a | Negative a
  deriving (Eq, Ord, Generic, Show, Read)

instance Functor Signed where
  fmap f (Positive x) = Positive (f x)
  fmap f (Negative x) = Negative (f x)

instance Traversable Signed where
  traverse f (Positive x) = Positive <$> f x
  traverse f (Negative x) = Negative <$> f x

instance Foldable Signed where
  foldMap = foldMapDefault

instance Applicative Signed where
  pure = Positive
  (<*>) = ap

instance Monad Signed where
  Positive x >>= f = f x
  Negative x >>= f = negateSigned $ f x

infix 9 /\
infix 9 \/

-- | A boolean type class.
class Boolean f where
  (/\) :: f a -> f a -> f a
  (\/) :: f a -> f a -> f a
  bNot :: f a -> f a
  bTrue :: f a
  bFalse :: f a
  bConst :: Signed a -> f a

-- | Generalized 'Data.Foldable.and'.
bAnd :: (Foldable t, Boolean f) => t (f b) -> f b
bAnd = foldr (/\) bTrue

-- | Generalized 'Data.Foldable.all'.
bAll :: (Foldable t, Boolean f) => (a -> f b) -> t a -> f b
bAll f = foldr (\x y -> f x /\ y) bTrue

-- | Generalized 'Data.Foldable.or'.
bOr :: (Foldable t, Boolean f) => t (f b) -> f b
bOr = foldr (\/) bFalse

-- | Generalized 'Data.Foldable.any'.
bAny :: (Foldable t, Boolean f) => (a -> f b) -> t a -> f b
bAny f = foldr (\x y -> f x \/ y) bFalse

-- | Syntax of boolean expressions parameterized over a
-- set of leaves, named constants.
data BoolExpr a
  = BAnd (BoolExpr a) (BoolExpr a)
  | BOr (BoolExpr a) (BoolExpr a)
  | BNot (BoolExpr a)
  | BTrue
  | BFalse
  | BConst (Signed a)
  deriving (Eq, Ord, Generic, Show {-! derive : Arbitrary !-})

prerequisiteOptions :: Options
prerequisiteOptions =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    , constructorTagModifier = map toLower
    }

instance (ToJSON a) => ToJSON (Signed a) where
  toJSON = genericToJSON prerequisiteOptions

instance (ToJSON a) => ToJSON (BoolExpr a) where
  toJSON = genericToJSON prerequisiteOptions

instance (ToJSON a) => ToJSON (DNF a) where
  toJSON = genericToJSON prerequisiteOptions

instance (ToJSON a) => ToJSON (CNF a) where
  toJSON = genericToJSON prerequisiteOptions

instance (ToJSON a) => ToJSON (Conj a) where
  toJSON = genericToJSON prerequisiteOptions

instance (ToJSON a) => ToJSON (Disj a) where
  toJSON = genericToJSON prerequisiteOptions

instance Functor BoolExpr where
  fmap f (BAnd a b) = BAnd (fmap f a) (fmap f b)
  fmap f (BOr a b) = BOr (fmap f a) (fmap f b)
  fmap f (BNot t) = BNot (fmap f t)
  fmap _ BTrue = BTrue
  fmap _ BFalse = BFalse
  fmap f (BConst x) = BConst (fmap f x)

instance Traversable BoolExpr where
  traverse f (BAnd a b) = BAnd <$> traverse f a <*> traverse f b
  traverse f (BOr a b) = BOr <$> traverse f a <*> traverse f b
  traverse f (BNot t) = BNot <$> traverse f t
  traverse _ BTrue = pure BTrue
  traverse _ BFalse = pure BFalse
  traverse f (BConst x) = BConst <$> traverse f x

instance Foldable BoolExpr where
  foldMap = foldMapDefault

instance Boolean BoolExpr where
  (/\) = BAnd
  (\/) = BOr
  bNot = BNot
  bTrue = BTrue
  bFalse = BFalse
  bConst = BConst

newtype Eval b a = Eval {runEval :: (a -> b) -> b}

runEvalId :: Eval a a -> a
runEvalId e = runEval e id

instance b ~ Bool => Boolean (Eval b) where
  (/\) = liftE2 (&&)
  (\/) = liftE2 (||)
  bNot = liftE not
  bTrue = Eval $ const True
  bFalse = Eval $ const False
  bConst = Eval . flip evalSigned

liftE :: (b -> b) -> Eval b a -> Eval b a
liftE f (Eval x) = Eval (f . x)

liftE2 :: (b -> b -> b) -> Eval b a -> Eval b a -> Eval b a
liftE2 f (Eval x) (Eval y) = Eval (\e -> f (x e) (y e))

-- | Turns a boolean tree into any boolean type.
fromBoolExpr :: Boolean f => BoolExpr a -> f a
fromBoolExpr (BAnd l r) = fromBoolExpr l /\ fromBoolExpr r
fromBoolExpr (BOr l r) = fromBoolExpr l \/ fromBoolExpr r
fromBoolExpr (BNot t) = bNot $ fromBoolExpr t
fromBoolExpr BTrue = bTrue
fromBoolExpr BFalse = bFalse
fromBoolExpr (BConst c) = bConst c

--- | Disjunction of atoms ('a')
newtype Disj a = Disj {unDisj :: [a]}
  deriving (Show, Generic, Functor, Semigroup, Monoid)

--- | Conjunction of atoms ('a')
newtype Conj a = Conj {unConj :: [a]}
  deriving (Show, Generic, Functor, Semigroup, Monoid)

--- | Conjunctive Normal Form
newtype CNF a = CNF {unCNF :: Conj (Disj (Signed a))}
  deriving (Show, Generic, Semigroup, Monoid)

--- | Disjunctive Normal Form
newtype DNF a = DNF {unDNF :: Disj (Conj (Signed a))}
  deriving (Show, Generic, Semigroup, Monoid)

instance Functor CNF where
  fmap f (CNF x) = CNF (fmap (fmap (fmap f)) x)

instance Boolean CNF where
  l /\ r = l `mappend` r
  l \/ r =
    CNF $
      Conj
        [ x `mappend` y | x <- unConj $ unCNF l, y <- unConj $ unCNF r
        ]
  bNot = error "bNot on CNF"
  bTrue = CNF $ Conj []
  bFalse = CNF $ Conj [Disj []]
  bConst x = CNF $ Conj [Disj [x]]

instance Functor DNF where
  fmap f (DNF x) = DNF (fmap (fmap (fmap f)) x)

instance Boolean DNF where
  l /\ r =
    DNF $
      Disj
        [ x `mappend` y | x <- unDisj $ unDNF l, y <- unDisj $ unDNF r
        ]
  l \/ r = l `mappend` r
  bNot = error "bNot on CNF"
  bTrue = DNF $ Disj [Conj []]
  bFalse = DNF $ Disj []
  bConst x = DNF $ Disj [Conj [x]]

-- | Reduce a boolean tree annotated by booleans to a single boolean.
reduceBoolExpr :: BoolExpr Bool -> Bool
reduceBoolExpr = evalBoolExpr id

-- Given a evaluation function of constants, returns an evaluation
-- function over boolean trees.
--
-- Note that since 'BoolExpr' is a functor, one can simply use
-- 'reduceBoolExpr':
--
-- @
-- evalBoolExpr f = reduceBoolExpr . fmap (f$)
-- @
evalBoolExpr :: (a -> Bool) -> (BoolExpr a -> Bool)
evalBoolExpr env expr = runEval (fromBoolExpr expr) env

-- | Returns constants used in a given boolean tree, these
-- constants are returned signed depending one how many
-- negations stands over a given constant.
constants :: BoolExpr a -> [Signed a]
constants = go True
 where
  go sign (BAnd a b) = go sign a ++ go sign b
  go sign (BOr a b) = go sign a ++ go sign b
  go sign (BNot t) = go (not sign) t
  go _ BTrue = []
  go _ BFalse = []
  go sign (BConst x) = [if sign then x else negateSigned x]

dualize :: Boolean f => BoolExpr a -> f a
dualize (BAnd l r) = dualize l \/ dualize r
dualize (BOr l r) = dualize l /\ dualize r
dualize BTrue = bFalse
dualize BFalse = bTrue
dualize (BConst c) = negateConstant c
dualize (BNot e) = fromBoolExpr e

-- When dualize is used by pushNotInwards not BNot remain,
-- hence it makes sense to assert that dualize does not
-- have to work on BNot. However `dualize` can be freely
-- used as a fancy `bNot`.
-- dualize (BNot _)   = error "dualize: impossible"

-- | Push the negations inwards as much as possible.
-- The resulting boolean tree no longer use negations.
pushNotInwards :: Boolean f => BoolExpr a -> f a
pushNotInwards (BAnd l r) = pushNotInwards l /\ pushNotInwards r
pushNotInwards (BOr l r) = pushNotInwards l \/ pushNotInwards r
pushNotInwards (BNot t) = dualize $ pushNotInwards t
pushNotInwards BTrue = bTrue
pushNotInwards BFalse = bFalse
pushNotInwards (BConst c) = bConst c

-- | Convert a 'CNF' (a boolean expression in conjunctive normal form)
-- to any other form supported by 'Boolean'.
fromCNF :: Boolean f => CNF a -> f a
fromCNF = bAll (bAny bConst . unDisj) . unConj . unCNF

-- | Convert a 'DNF' (a boolean expression in disjunctive normal form)
-- to any other form supported by 'Boolean'.
fromDNF :: Boolean f => DNF a -> f a
fromDNF = bAny (bAll bConst . unConj) . unDisj . unDNF

-- | Convert a boolean tree to a conjunctive normal form.
boolTreeToCNF :: BoolExpr a -> CNF a
boolTreeToCNF = pushNotInwards

-- | Convert a boolean tree to a disjunctive normal form.
boolTreeToDNF :: BoolExpr a -> DNF a
boolTreeToDNF = pushNotInwards

-- | Reduce a boolean expression in conjunctive normal form to a single
-- boolean.
reduceCNF :: CNF Bool -> Bool
reduceCNF = runEvalId . fromCNF

-- | Reduce a boolean expression in disjunctive normal form to a single
-- boolean.
reduceDNF :: DNF Bool -> Bool
reduceDNF = runEvalId . fromDNF

evalSigned :: (a -> Bool) -> Signed a -> Bool
evalSigned f (Positive x) = f x
evalSigned f (Negative x) = not $ f x

reduceSigned :: Signed Bool -> Bool
reduceSigned = evalSigned id

negateSigned :: Signed a -> Signed a
negateSigned (Positive x) = Negative x
negateSigned (Negative x) = Positive x

negateConstant :: Boolean f => Signed a -> f a
negateConstant = bConst . negateSigned

{-
prop_reduceBoolExpr_EQ_reduceCNF t = reduceBoolExpr t == reduceCNF (boolTreeToCNF t)

prop_reduceBoolExpr_EQ_reduceCNF_Bool = prop_reduceBoolExpr_EQ_reduceCNF (BConst . not)

prop_reduceBoolExpr_EQ_reduceDNF t = reduceBoolExpr t == reduceDNF (boolTreeToDNF t)

prop_reduceBoolExpr_EQ_reduceDNF_Bool = prop_reduceBoolExpr_EQ_reduceDNF (BConst . not)

{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance (Arbitrary a) => Arbitrary (BoolExpr a) where
    arbitrary = do x <- choose (1::Int,6) -- :: Int inserted manually
                   case x of
                     1 -> do  v1 <- arbitrary
                              v2 <- arbitrary
                              return (BAnd v1 v2)
                     2 -> do  v1 <- arbitrary
                              v2 <- arbitrary
                              return (BOr v1 v2)
                     3 -> do  v1 <- arbitrary
                              return (BNot v1)
                     4 -> do  return (BTrue )
                     5 -> do  return (BFalse )
                     6 -> do  v1 <- arbitrary
                              return (BConst v1)
    --coarbitrary = error "coarbitrary not yet supported" -- quickcheck2
-}
