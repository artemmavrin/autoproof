{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof
  ( Formula,
    imp,
    isTautology,
    prettyFormula,
    proveImp,
    var,
    (|-),
  )
import AutoProof.Parser (unsafeParseFormula)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  -- Load known provable and unprovable sequents
  provable <- loadFormulas "test/spec/is-tautology/examples/provable.txt"
  unprovable <- loadFormulas "test/spec/is-tautology/examples/unprovable.txt"

  -- Run tests
  H.hspec $
    H.describe "isTautology" $ do
      mapM_ assertProvable provable
      mapM_ assertUnprovable unprovable
      assertConsistentWithProveImp

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

timeoutMicroseconds :: Int
timeoutMicroseconds = 60000000 -- One minute

wait :: QC.Testable p => p -> QC.Property
wait = QC.within timeoutMicroseconds

assertProvable :: Formula String -> H.SpecWith ()
assertProvable f =
  H.it ("tautology: " ++ prettyFormula f) $ wait $ isTautology f

assertUnprovable :: Formula String -> H.SpecWith ()
assertUnprovable f =
  H.it ("not a tautology: " ++ prettyFormula f) $ wait $ not $ isTautology f

-- Random implicational formula
formula :: Int -> QC.Gen (Formula Char)
formula n
  | n == 0 = variable
  | otherwise = QC.oneof [variable, implication]
  where
    variable = var <$> QC.chooseEnum ('a', 'd')
    implication = imp <$> subformula <*> subformula
    subformula = formula $ n `div` 2

instance QC.Arbitrary (Formula Char) where
  arbitrary = QC.sized formula

consistentWithProveImp :: Formula Char -> Bool
consistentWithProveImp a = isJust (proveImp ([] |- a)) == isTautology a

assertConsistentWithProveImp :: H.SpecWith ()
assertConsistentWithProveImp =
  H.it "checking consistency between proveImp and isTautology" $
    QC.withMaxSuccess 100 consistentWithProveImp
