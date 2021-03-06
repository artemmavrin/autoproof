{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof
  ( Formula,
    correct,
    prettyFormula,
    proveTautology,
    (|-),
  )
import AutoProof.Parser (unsafeParseFormula)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  -- Load known provable and unprovable formulas
  provable <- loadFormulas "test/data/formulas/tautologies.txt"
  unprovable <- loadFormulas "test/data/formulas/not-tautologies.txt"

  -- Run tests
  H.hspec $
    H.describe "proveTautology" $ do
      mapM_ assertProvable provable
      mapM_ assertUnprovable unprovable

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
assertProvable a =
  H.it ("tautology: " ++ prettyFormula a) $
    wait $
      let mp = proveTautology a
       in isJust mp && correct ([] |- a) (fromJust mp)

assertUnprovable :: Formula String -> H.SpecWith ()
assertUnprovable f =
  H.it ("not a tautology: " ++ prettyFormula f) $ wait $ isNothing $ proveTautology f
