{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof.Classical
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    isProvable,
    (|-),
  )
import qualified AutoProof.Classical.CNF as CNF (fromFormula, toFormula)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  H.hspec $
    H.describe "CNF" $ do
      validateFormulaEntailsCNF
      validateCNFEntailsFormula

formula :: Int -> QC.Gen (Formula Int)
formula n
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
    subformula = QC.chooseInt (0, n - 1) >>= formula
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
  arbitrary = QC.chooseInt (0, 2) >>= formula

timeoutMicroseconds :: Int
timeoutMicroseconds = 60000000 -- One minute

wait :: QC.Testable p => p -> QC.Property
wait = QC.within timeoutMicroseconds

assertFormulaEntailsCNF :: Formula Int -> Bool
assertFormulaEntailsCNF a = isProvable ([a] |- CNF.toFormula (CNF.fromFormula a))

assertCNFEntailsFormula :: Formula Int -> Bool
assertCNFEntailsFormula a = isProvable ([CNF.toFormula (CNF.fromFormula a)] |- a)

validateFormulaEntailsCNF :: H.SpecWith ()
validateFormulaEntailsCNF =
  H.it "a |- CNF(a)" $
    QC.withMaxSuccess 1000 assertFormulaEntailsCNF

validateCNFEntailsFormula :: H.SpecWith ()
validateCNFEntailsFormula =
  H.it "CNF(a) |- a" $
    QC.withMaxSuccess 1000 assertCNFEntailsFormula
