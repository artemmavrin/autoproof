{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof.Classical
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    canonicalCNF,
    isProvable,
    subformulas,
    substitute,
    (|-),
  )
import qualified AutoProof.Classical.CNF as CNF
  ( fromFormula,
    pureLiteral,
    substitute,
    unitLiteral,
  )
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map (findWithDefault, toList)
import qualified Data.Set as Set (member)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  H.hspec $
    H.describe "CNF" $ do
      validateCanonicalForm
      validateFormulaEntailsCNF
      validateCNFEntailsFormula
      validateSubstitution
      validateUnitLiteral
      validatePureLiteral

arbitraryFormula :: Int -> QC.Gen (Formula Int)
arbitraryFormula n
  | n == 0 = QC.oneof [literal, variable]
  | otherwise = QC.oneof formulas
  where
    literal = Lit <$> QC.arbitrary
    variable = Var <$> QC.chooseInt (0, 2)
    negation = Not <$> subformula
    implication = Imp <$> subformula <*> subformula
    disjunction = Or <$> subformula <*> subformula
    conjunction = And <$> subformula <*> subformula
    equivalence = Iff <$> subformula <*> subformula
    subformula = QC.chooseInt (0, n - 1) >>= arbitraryFormula
    formulas =
      [ literal,
        variable,
        negation,
        implication,
        disjunction,
        conjunction,
        equivalence
      ]

instance QC.Arbitrary (Formula Int) where
  arbitrary = QC.chooseInt (0, 2) >>= arbitraryFormula

timeoutMicroseconds :: Int
timeoutMicroseconds = 60000000 -- One minute

wait :: QC.Testable p => p -> QC.Property
wait = QC.within timeoutMicroseconds

assertFormulaEntailsCNF :: Formula Int -> QC.Property
assertFormulaEntailsCNF a = wait $ isProvable ([a] |- canonicalCNF a)

assertCNFEntailsFormula :: Formula Int -> QC.Property
assertCNFEntailsFormula a = wait $ isProvable ([canonicalCNF a] |- a)

assertCanonicalForm :: Formula Int -> Bool
assertCanonicalForm a = noTrueOrFalse (canonicalCNF a)

assertCorrectUnitLiteral :: Formula Int -> QC.Property
assertCorrectUnitLiteral a =
  let cnf = CNF.fromFormula a
      l' = CNF.unitLiteral cnf
   in isJust l'
        QC.==> let (x, b) = fromJust l'
                in any ((== [(x, b)]) . Map.toList) cnf

assertCorrectPureLiteral :: Formula Int -> QC.Property
assertCorrectPureLiteral a =
  let cnf = CNF.fromFormula a
      l' = CNF.pureLiteral cnf
   in isJust l'
        QC.==> let (x, b) = fromJust l'
                in all ((== b) . Map.findWithDefault b x) cnf

noTrueOrFalse :: Ord b => Formula b -> Bool
noTrueOrFalse a =
  a == Lit True || a == Lit False
    || not (Lit True `Set.member` s || Lit False `Set.member` s)
  where
    s = subformulas a

assertSubstitution :: Formula Int -> Bool -> Bool
assertSubstitution a b =
  CNF.substitute (CNF.fromFormula a) 0 b
    == CNF.fromFormula (substitute a 0 (Lit b))

validateFormulaEntailsCNF :: H.SpecWith ()
validateFormulaEntailsCNF =
  H.it "a |- CNF(a)" $
    QC.withMaxSuccess 10000 assertFormulaEntailsCNF

validateCNFEntailsFormula :: H.SpecWith ()
validateCNFEntailsFormula =
  H.it "CNF(a) |- a" $
    QC.withMaxSuccess 10000 assertCNFEntailsFormula

validateCanonicalForm :: H.SpecWith ()
validateCanonicalForm =
  H.it "computed CNFs are in canonical form" $
    QC.withMaxSuccess 10000 assertCanonicalForm

validateSubstitution :: H.SpecWith ()
validateSubstitution =
  H.it "CNF conversion \"commutes\" with substitutions" $
    QC.withMaxSuccess 10000 assertSubstitution

validateUnitLiteral :: H.SpecWith ()
validateUnitLiteral =
  H.it "get unit literal" $
    QC.withMaxSuccess 10000 assertCorrectUnitLiteral

validatePureLiteral :: H.SpecWith ()
validatePureLiteral =
  H.it "get pure literal" $
    QC.withMaxSuccess 10000 assertCorrectPureLiteral
