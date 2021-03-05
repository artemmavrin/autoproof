{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AutoProof
  ( Formula,
    Judgement,
    correct,
    imp,
    proveImp,
    strengthenProof,
    var,
    (|-),
  )
import Data.Maybe (fromJust, isJust)
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = H.hspec $
  H.describe "strengthenProof" $ do
    validateRandomValidProofOfTautology
    validateRandomValidProofOfJudgement

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

instance QC.Arbitrary (Judgement Char) where
  arbitrary = (|-) <$> (QC.chooseInt (0, 2) >>= QC.vector) <*> QC.arbitrary

timeoutMicroseconds :: Int
timeoutMicroseconds = 60000000 -- One minute

wait :: QC.Testable p => p -> QC.Property
wait = QC.within timeoutMicroseconds

assertValidProofOfTautology :: Formula Char -> QC.Property
assertValidProofOfTautology t = assertValidProofOfJudgement ([] |- t)

assertValidProofOfJudgement :: Judgement Char -> QC.Property
assertValidProofOfJudgement j =
  let mp = proveImp j
   in wait $
        isJust mp
          QC.==> let p = fromJust mp
                     p' = strengthenProof p
                  in correct j p'

validateRandomValidProofOfTautology :: H.SpecWith ()
validateRandomValidProofOfTautology =
  H.it "trying to prove and strengthen random tautologies" $
    QC.withMaxSuccess 10000 assertValidProofOfTautology

validateRandomValidProofOfJudgement :: H.SpecWith ()
validateRandomValidProofOfJudgement =
  H.it "trying to prove and strengthen random judgements" $
    QC.withMaxSuccess 10000 assertValidProofOfJudgement
