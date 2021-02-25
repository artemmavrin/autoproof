module Main where

import AutoProof
  ( Formula,
    isTautology,
    prettyFormula,
  )
import AutoProof.Parser (unsafeParseFormula)
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

loadFormulas :: String -> IO [Formula String]
loadFormulas filename = do
  contents <- readFile filename
  return (unsafeParseFormula <$> lines contents)

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
