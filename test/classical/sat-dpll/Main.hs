{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof.Classical
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    pretty,
    satAssignmentDPLL,
    satDPLL,
    (|=),
  )
import AutoProof.Internal.Parser (unsafeParseFormula)
import AutoProof.Internal.Utils.PrettyPrintable (PrettyPrintable)
import Data.Char (isSpace)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  provableFormulas <- loadFormulas "test/data/formulas/classical-only-tautologies.txt"
  unprovableFormulas <- loadFormulas "test/data/formulas/classical-unprovable.txt"

  H.hspec $
    H.describe "DPLL SAT" $ do
      mapM_ assertSatisfiable provableFormulas
      mapM_ (assertSatisfiable . Not) unprovableFormulas
      mapM_ (assertUnsatisfiable . Not) provableFormulas
      checkRandomSATCases

loadFormulas :: String -> IO [Formula String]
loadFormulas filename = do
  contents <- readFile filename
  let strings = lines contents
  let formulaStrings = clean strings
  return (unsafeParseFormula <$> formulaStrings)
  where
    stripSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    dropComments = takeWhile (/= '#')
    clean = filter (not . null) . map (stripSpace . dropComments)

arbitraryFormula :: Int -> QC.Gen (Formula Int)
arbitraryFormula n
  | n == 0 = QC.oneof [literal, variable]
  | otherwise = QC.oneof formulas
  where
    literal = Lit <$> QC.arbitrary
    variable = Var <$> QC.chooseInt (0, 4)
    negation = Not <$> subformula
    implication = Imp <$> subformula <*> subformula
    disjunction = Or <$> subformula <*> subformula
    conjunction = And <$> subformula <*> subformula
    equivalence = Iff <$> subformula <*> subformula
    subformula = QC.chooseInt (0, n `div` 2) >>= arbitraryFormula
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
  arbitrary = QC.sized arbitraryFormula

timeoutMicroseconds :: Int
timeoutMicroseconds = 60000000 -- One minute

wait :: QC.Testable p => p -> QC.Property
wait = QC.within timeoutMicroseconds

assertSATCase :: Formula Int -> QC.Property
assertSATCase a =
  wait $
    if satDPLL a
      then case satAssignmentDPLL a of
        Just m -> m |= a
        _ -> False
      else case satAssignmentDPLL a of
        Nothing -> True
        _ -> False

assertSatisfiable :: (PrettyPrintable a, Ord a) => Formula a -> H.SpecWith ()
assertSatisfiable a =
  H.it ("satisfiable: " ++ pretty a) $
    wait $
      satDPLL a
        && case satAssignmentDPLL a of
          Just m -> m |= a
          _ -> False

assertUnsatisfiable :: (PrettyPrintable a, Ord a) => Formula a -> H.SpecWith ()
assertUnsatisfiable a =
  H.it ("unsatisfiable: " ++ pretty a) $
    wait $
      not (satDPLL a)
        && case satAssignmentDPLL a of
          Nothing -> True
          _ -> False

checkRandomSATCases :: H.SpecWith ()
checkRandomSATCases =
  H.it "checking satisfiability of random formulas" $
    QC.withMaxSuccess 10000 assertSATCase
