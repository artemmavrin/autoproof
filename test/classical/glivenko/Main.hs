module Main where

import AutoProof.Classical
  ( Formula,
    Judgement,
    pretty,
    (|-),
  )
import AutoProof.Classical.Proof.Glivenko (isProvableGlivenko)
import AutoProof.Internal.Parser (unsafeParseFormula, unsafeParseJudgement)
import Data.Char (isSpace)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  -- Load known provable and unprovable formulas and judgements
  provableFormulas <- loadFormulas "test/data/formulas/classical-only-tautologies.txt"
  unprovableFormulas <- loadFormulas "test/data/formulas/classical-unprovable.txt"
  provableJudgements <- loadJudgements "test/data/judgements/classical-only-provable.txt"
  unprovableJudgements <- loadJudgements "test/data/judgements/classical-unprovable.txt"

  -- Run tests
  H.hspec $
    H.describe "isProvableGlivenko" $ do
      mapM_ assertProvableFormula provableFormulas
      mapM_ assertUnprovableFormula unprovableFormulas
      mapM_ assertProvableJudgement provableJudgements
      mapM_ assertUnprovableJudgement unprovableJudgements

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

loadJudgements :: String -> IO [Judgement String]
loadJudgements filename = do
  contents <- readFile filename
  let strings = lines contents
  let judgementStrings = clean strings
  return (unsafeParseJudgement <$> judgementStrings)
  where
    stripSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    dropComments = takeWhile (/= '#')
    clean = filter (not . null) . map (stripSpace . dropComments)

timeoutMicroseconds :: Int
timeoutMicroseconds = 60000000 -- One minute

wait :: QC.Testable p => p -> QC.Property
wait = QC.within timeoutMicroseconds

assertProvableJudgement :: Judgement String -> H.SpecWith ()
assertProvableJudgement j =
  H.it ("provable: " ++ pretty j) $ wait $ isProvableGlivenko j

assertUnprovableJudgement :: Judgement String -> H.SpecWith ()
assertUnprovableJudgement j =
  H.it ("not provable: " ++ pretty j) $ wait $ not (isProvableGlivenko j)

assertProvableFormula :: Formula String -> H.SpecWith ()
assertProvableFormula a = assertProvableJudgement $ [] |- a

assertUnprovableFormula :: Formula String -> H.SpecWith ()
assertUnprovableFormula a = assertUnprovableJudgement $ [] |- a
