{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    Judgement,
    Proof,
    andElimL,
    andElimR,
    andIntr,
    axiom,
    falseElim,
    iffElimL,
    iffElimR,
    iffIntr,
    impElim,
    impIntr,
    notElim,
    notIntr,
    orElim,
    orIntrL,
    orIntrR,
    trueIntr,
    (|-),
  )
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = H.hspec $ do
  H.describe "instance Eq (Proof a)" $ do
    assertEqReflexive
    assertEqSymmetric
    assertEqTransitive
  H.describe "instance Ord (Proof a)" $ do
    assertOrdReflexive
    assertOrdAntisymmetric
    assertOrdTransitive
    assertOrdConsistent
    assertOrdConsistentStrict
    assertOrdEquality
    assertOrdStrictness

-- Generation of random formulas and proofs for testing

formula :: Int -> QC.Gen (Formula Int)
formula n
  | n == 0 = QC.oneof [literal, variable]
  | otherwise = QC.oneof formulas
  where
    formulas = [literal, variable, negation, implication, disjunction, conjunction, equivalence]
    literal = Lit <$> QC.arbitrary
    variable = Var <$> QC.chooseInt (0, 2)
    negation = Not <$> subformula
    implication = Imp <$> subformula <*> subformula
    disjunction = Or <$> subformula <*> subformula
    conjunction = And <$> subformula <*> subformula
    equivalence = Iff <$> subformula <*> subformula
    subformula = formula $ n `div` 2

instance QC.Arbitrary (Formula Int) where
  arbitrary = QC.sized formula

instance QC.Arbitrary (Judgement Int) where
  arbitrary = (|-) <$> (QC.chooseInt (0, 3) >>= QC.vector) <*> QC.arbitrary

-- These random proofs are by no means valid!!!

proof :: Int -> QC.Gen (Proof Int)
proof n
  | n == 0 = QC.oneof [ax, tIntr]
  | otherwise = QC.oneof proofs
  where
    ax = axiom <$> QC.arbitrary
    fElim = falseElim <$> QC.arbitrary <*> subproof
    tIntr = trueIntr <$> QC.arbitrary
    nElim = notElim <$> QC.arbitrary <*> subproof <*> subproof
    nIntr = notIntr <$> QC.arbitrary <*> subproof
    iElim = impElim <$> QC.arbitrary <*> subproof <*> subproof
    iIntr = impIntr <$> QC.arbitrary <*> subproof
    oElim = orElim <$> QC.arbitrary <*> subproof <*> subproof <*> subproof
    oIntrL = orIntrL <$> QC.arbitrary <*> subproof
    oIntrR = orIntrR <$> QC.arbitrary <*> subproof
    aElimL = andElimL <$> QC.arbitrary <*> subproof
    aElimR = andElimR <$> QC.arbitrary <*> subproof
    aIntr = andIntr <$> QC.arbitrary <*> subproof <*> subproof
    eElimL = iffElimL <$> QC.arbitrary <*> subproof
    eElimR = iffElimR <$> QC.arbitrary <*> subproof
    eIntr = iffIntr <$> QC.arbitrary <*> subproof <*> subproof
    subproof = proof $ n `div` 2
    proofs =
      [ ax,
        tIntr,
        fElim,
        nElim,
        nIntr,
        iElim,
        iIntr,
        oElim,
        oIntrL,
        oIntrR,
        aElimL,
        aElimR,
        aIntr,
        eElimL,
        eElimR,
        eIntr
      ]

instance QC.Arbitrary (Proof Int) where
  arbitrary = QC.sized proof

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

-- Testable assertions

makeAssertion :: QC.Testable p => String -> Int -> p -> H.SpecWith ()
makeAssertion label maxS prop = H.it label $ QC.withMaxSuccess maxS prop

assertEqReflexive :: H.SpecWith ()
assertEqReflexive =
  makeAssertion
    "reflexive: p == p"
    10000
    (eqReflexive :: Proof Int -> Bool)

assertEqSymmetric :: H.SpecWith ()
assertEqSymmetric =
  makeAssertion
    "symmetric: p == q if and only if q == p"
    10000
    (eqSymmetric :: Proof Int -> Proof Int -> Bool)

assertEqTransitive :: H.SpecWith ()
assertEqTransitive =
  makeAssertion
    "transitive: if p == q and q == r then p == r"
    10000
    (eqTransitive :: Proof Int -> Proof Int -> Proof Int -> Bool)

assertOrdReflexive :: H.SpecWith ()
assertOrdReflexive =
  makeAssertion
    "reflexive: p <= p"
    10000
    (ordReflexive :: Proof Int -> Bool)

assertOrdAntisymmetric :: H.SpecWith ()
assertOrdAntisymmetric =
  makeAssertion
    "antisymmetric: if p <= q and q <= p then p == q"
    10000
    (ordAntisymmetric :: Proof Int -> Proof Int -> Bool)

assertOrdTransitive :: H.SpecWith ()
assertOrdTransitive =
  makeAssertion
    "transitive: if p <= q and q <= r then p <= r"
    10000
    (ordTransitive :: Proof Int -> Proof Int -> Proof Int -> Bool)

assertOrdConsistent :: H.SpecWith ()
assertOrdConsistent =
  makeAssertion
    "consistency: p <= q if and only if q >= p"
    10000
    (ordConsistent :: Proof Int -> Proof Int -> Bool)

assertOrdConsistentStrict :: H.SpecWith ()
assertOrdConsistentStrict =
  makeAssertion
    "consistency (strict): p < q if and only if q > p"
    10000
    (ordConsistentStrict :: Proof Int -> Proof Int -> Bool)

assertOrdEquality :: H.SpecWith ()
assertOrdEquality =
  makeAssertion
    "equality: p == q if and only if compare p q == EQ"
    10000
    (ordEquality :: Proof Int -> Proof Int -> Bool)

assertOrdStrictness :: H.SpecWith ()
assertOrdStrictness =
  makeAssertion
    "strict inequality: p < q if and only if p <= q and p /= q"
    10000
    (ordStrictness :: Proof Int -> Proof Int -> Bool)
