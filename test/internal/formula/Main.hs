{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    children,
    subformulas,
  )
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = H.hspec $ do
  H.describe "instance Eq (Formula a)" $ do
    assertEqReflexive
    assertEqSymmetric
    assertEqTransitive
  H.describe "instance Ord (Formula a)" $ do
    assertOrdReflexive
    assertOrdAntisymmetric
    assertOrdTransitive
    assertOrdConsistent
    assertOrdConsistentStrict
    assertOrdEquality
    assertOrdStrictness
    assertChildrenSmallerThanParents
  H.describe "instance Show (Formula a), Read (Formula a)" $ do
    assertReadShow
  H.describe "subformulas" $ do
    assertSubformulasLTE

-- Generation of random formulas for testing

formula :: Int -> QC.Gen (Formula Int)
formula n
  | n == 0 = QC.oneof [literal, variable]
  | otherwise = QC.oneof formulas
  where
    formulas = [literal, variable, negation, implication, disjunction, conjunction, equivalence]
    literal = Lit <$> QC.arbitrary
    variable = Var <$> QC.chooseInt (0, 9)
    negation = Not <$> subformula
    implication = Imp <$> subformula <*> subformula
    disjunction = Or <$> subformula <*> subformula
    conjunction = And <$> subformula <*> subformula
    equivalence = Iff <$> subformula <*> subformula
    subformula = formula $ n `div` 2

instance QC.Arbitrary (Formula Int) where
  arbitrary = QC.sized formula

-- Properties to be tested

eqReflexive :: Eq a => a -> Bool
eqReflexive x = x == x

eqSymmetric :: Eq a => a -> a -> Bool
eqSymmetric x y = (x == y) == (y == x)

eqTransitive :: Eq a => a -> a -> a -> Bool
eqTransitive x y z = if x == y && y == z then x == z else True

ordReflexive :: Ord a => a -> Bool
ordReflexive x = x <= x

ordAntisymmetric :: Ord a => a -> a -> Bool
ordAntisymmetric x y = (x <= y && y <= x) == (x == y)

ordTransitive :: Ord a => a -> a -> a -> Bool
ordTransitive x y z = if x <= y && y <= z then x <= z else True

ordConsistent :: Ord a => a -> a -> Bool
ordConsistent x y = (x <= y) == (y >= x)

ordConsistentStrict :: Ord a => a -> a -> Bool
ordConsistentStrict x y = (x < y) == (y > x)

ordEquality :: Ord a => a -> a -> Bool
ordEquality x y = (x == y) == (compare x y == EQ)

ordStrictness :: Ord a => a -> a -> Bool
ordStrictness x y = (x < y) == (x <= y && x /= y)

subformulasLTE :: Formula Int -> Bool
subformulasLTE p = all (<= p) $ subformulas p

readShow :: Int -> Formula Int -> Bool
readShow d x = (x, "") `elem` readsPrec d (showsPrec d x "")

childrenSmallerThanParents :: Formula Int -> Bool
childrenSmallerThanParents a = all (< a) (children a)

-- Testable assertions

makeAssertion :: QC.Testable p => String -> Int -> p -> H.SpecWith ()
makeAssertion label maxS prop = H.it label $ QC.withMaxSuccess maxS prop

assertEqReflexive :: H.SpecWith ()
assertEqReflexive =
  makeAssertion
    "reflexive: p == p"
    10000
    (eqReflexive :: Formula Int -> Bool)

assertEqSymmetric :: H.SpecWith ()
assertEqSymmetric =
  makeAssertion
    "symmetric: p == q if and only if q == p"
    10000
    (eqSymmetric :: Formula Int -> Formula Int -> Bool)

assertEqTransitive :: H.SpecWith ()
assertEqTransitive =
  makeAssertion
    "transitive: if p == q and q == r then p == r"
    10000
    (eqTransitive :: Formula Int -> Formula Int -> Formula Int -> Bool)

assertOrdReflexive :: H.SpecWith ()
assertOrdReflexive =
  makeAssertion
    "reflexive: p <= p"
    10000
    (ordReflexive :: Formula Int -> Bool)

assertOrdAntisymmetric :: H.SpecWith ()
assertOrdAntisymmetric =
  makeAssertion
    "antisymmetric: if p <= q and q <= p then p == q"
    10000
    (ordAntisymmetric :: Formula Int -> Formula Int -> Bool)

assertOrdTransitive :: H.SpecWith ()
assertOrdTransitive =
  makeAssertion
    "transitive: if p <= q and q <= r then p <= r"
    10000
    (ordTransitive :: Formula Int -> Formula Int -> Formula Int -> Bool)

assertOrdConsistent :: H.SpecWith ()
assertOrdConsistent =
  makeAssertion
    "consistency: p <= q if and only if q >= p"
    10000
    (ordConsistent :: Formula Int -> Formula Int -> Bool)

assertOrdConsistentStrict :: H.SpecWith ()
assertOrdConsistentStrict =
  makeAssertion
    "consistency (strict): p < q if and only if q > p"
    10000
    (ordConsistentStrict :: Formula Int -> Formula Int -> Bool)

assertOrdEquality :: H.SpecWith ()
assertOrdEquality =
  makeAssertion
    "equality: p == q if and only if compare p q == EQ"
    10000
    (ordEquality :: Formula Int -> Formula Int -> Bool)

assertOrdStrictness :: H.SpecWith ()
assertOrdStrictness =
  makeAssertion
    "strict inequality: p < q if and only if p <= q and p /= q"
    10000
    (ordStrictness :: Formula Int -> Formula Int -> Bool)

assertSubformulasLTE :: H.SpecWith ()
assertSubformulasLTE =
  makeAssertion
    "if q is a subformula of p, then q <= p"
    10000
    subformulasLTE

assertReadShow :: H.SpecWith ()
assertReadShow =
  makeAssertion
    "(x,\"\") is an element of (readsPrec d (showsPrec d x \"\"))"
    10000
    readShow

assertChildrenSmallerThanParents :: H.SpecWith ()
assertChildrenSmallerThanParents =
  makeAssertion
    "children of a formula should be ordered less than the formula itself"
    1000
    childrenSmallerThanParents
