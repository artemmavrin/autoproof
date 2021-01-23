{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Prop (Formula (Var, Imp), parseFormula)
import Data.Prop.Utils (PrettyPrintable (pretty))
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = H.hspec $
  H.describe "parseFormula" $
    H.it "Parsing a pretty-printed formula results in the original term" $
      QC.property $ \t -> (parseFormula . pretty) t == Right t

-- Wrapper type for variable names
newtype Name = Name {unName :: String} deriving (Eq, Show)

-- Candidate variable names for randomly generated variables
names :: [Name]
names = map Name strings
  where
    strings, oneChar, twoChar, primed :: [String]
    strings = oneChar ++ twoChar ++ primed
    oneChar = map return ['a' .. 'z']
    twoChar = zipWith (++) oneChar (reverse oneChar)
    primed = map (++ "'") (oneChar ++ twoChar)

instance QC.Arbitrary Name where
  arbitrary = QC.elements names

-- Generate a random random formula with a given "complexity"
formula :: Int -> QC.Gen (Formula String)
formula n
  | n == 0 = variable
  | otherwise = QC.oneof [variable, implication]
  where
    variable = Var . unName <$> QC.arbitrary
    implication = Imp <$> subformula <*> subformula
    subformula = formula $ n `div` 2

instance QC.Arbitrary (Formula String) where
  arbitrary = QC.sized formula
