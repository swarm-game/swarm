
module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Data.Set              as S

import           SubsetSolver

------------------------------------------------------------

data Var = A | B | C | D | E
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
instance Arbitrary Var where
  arbitrary = elements [A .. E]

data Constant = V | W | X | Y | Z
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
instance Arbitrary Constant where
  arbitrary = elements [V .. Z]

instance (Arbitrary v, Arbitrary c, Ord v, Ord c) => Arbitrary (Subset v c) where
  arbitrary =
    Subset
      <$> (S.fromList <$> listOf arbitrary)
      <*> (S.fromList <$> listOf arbitrary)
  shrink (Subset vs cs) =
    [ Subset vs' cs | vs' <- shrink vs ] ++ [ Subset vs cs' | cs' <- shrink cs ]

instance (Arbitrary v, Arbitrary c, Ord v, Ord c) => Arbitrary (Ineq v c) where
  arbitrary = (:<:) <$> arbitrary <*> arbitrary

  shrink (s1 :<: s2) =
    [ s1' :<: s2 | s1' <- shrink s1 ] ++ [ s1 :<: s2' | s2' <- shrink s2 ]

------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testProperty "the thing" prop_solve_is_solution
  ]

prop_solve_is_solution :: System Var Constant -> Bool
prop_solve_is_solution sys = case solve sys of
  Nothing -> True
  Just m  -> all (evalIneq m) sys
