{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Either (isRight)
import Data.Prop (Formula (And, Imp, Or, Var), parseFormula)
import Data.Prop.Utils (PrettyPrintable (pretty))
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  goodFormulas <- lines <$> readFile "test/parser/examples/propositions"
  H.hspec $
    H.describe "parseFormula" $ do
      mapM_ assertSuccessfulParse goodFormulas
      assertParsesPrettyRandom

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
  | n == 0 = variable
  | otherwise = QC.oneof [variable, implication, disjunction, conjunction]
  where
    variable = Var <$> QC.elements names
    implication = Imp <$> subformula <*> subformula
    disjunction = Or <$> subformula <*> subformula
    conjunction = And <$> subformula <*> subformula
    subformula = formula $ n `div` 2

-- Enable random formula generation for testing
instance QC.Arbitrary (Formula String) where
  arbitrary = QC.sized formula

assertParsesPrettyRandom :: H.SpecWith ()
assertParsesPrettyRandom =
  H.it "Parsing a pretty-printed formula results in the original term" $
    QC.property $ \t -> (parseFormula . pretty) t == Right t

assertSuccessfulParse :: String -> H.SpecWith ()
assertSuccessfulParse s =
  H.it ("should parse: " ++ show s) $
    isRight (parseFormula s)
