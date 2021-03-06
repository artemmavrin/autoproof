{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof
  ( Formula,
    and,
    iff,
    imp,
    lit,
    not,
    or,
    parseFormula,
    prettyFormula,
    var,
  )
import Data.Maybe (isJust)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import Prelude hiding (and, not, or)

main :: IO ()
main = do
  goodFormulas <- lines <$> readFile "test/spec/parser/examples/propositions.txt"
  H.hspec $
    H.describe "parseFormula" $ do
      mapM_ assertSuccessfulParse goodFormulas
      assertParsesPretty

-- Candidate variable names for random formulas
names :: [String]
names = oneChar ++ twoChar ++ primed
  where
    oneChar, twoChar, primed :: [String]
    oneChar = map return ['a' .. 'z']
    twoChar = zipWith (++) oneChar (reverse oneChar)
    primed = map (++ "'") (oneChar ++ twoChar)

-- Generate a random random formula with a given "complexity"
formula :: Int -> QC.Gen (Formula String)
formula n
  | n == 0 = QC.oneof [literal, variable]
  | otherwise = QC.oneof formulas
  where
    formulas = [literal, variable, negation, implication, disjunction, conjunction, equivalence]
    literal = lit <$> QC.arbitrary
    variable = var <$> QC.elements names
    negation = not <$> subformula
    implication = imp <$> subformula <*> subformula
    disjunction = or <$> subformula <*> subformula
    conjunction = and <$> subformula <*> subformula
    equivalence = iff <$> subformula <*> subformula
    subformula = formula $ n `div` 2

-- Enable random formula generation for testing
instance QC.Arbitrary (Formula String) where
  arbitrary = QC.sized formula

parsesPretty :: Formula String -> Bool
parsesPretty p = (parseFormula . prettyFormula) p == Just p

makeAssertion :: QC.Testable p => String -> Int -> p -> H.SpecWith ()
makeAssertion label maxS prop = H.it label $ QC.withMaxSuccess maxS prop

assertParsesPretty :: H.SpecWith ()
assertParsesPretty =
  makeAssertion
    "Parsing a random pretty-printed formula results in the original formula"
    10000
    parsesPretty

assertSuccessfulParse :: String -> H.SpecWith ()
assertSuccessfulParse s =
  H.it ("should parse: " ++ show s) $
    s `H.shouldSatisfy` (isJust . parseFormula)
