module Main where

import qualified AutoProof.Internal.Utils.DList as DList
  ( cons,
    fromList,
    toList,
  )
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

main :: IO ()
main = H.hspec $ do
  H.describe "DList" $ do
    assertConversion
    assertConcatDList
    assertConsDList

-- Properties to be tested

conversion :: Eq a => [a] -> Bool
conversion xs = xs == DList.toList (DList.fromList xs)

concatDLists :: Eq a => [a] -> [a] -> Bool
concatDLists xs ys = xs ++ ys == DList.toList (DList.fromList xs . DList.fromList ys)

consDLists :: Eq a => a -> [a] -> Bool
consDLists x xs = x : xs == DList.toList (DList.cons x (DList.fromList xs))

-- Testable assertions

makeAssertion :: QC.Testable p => String -> Int -> p -> H.SpecWith ()
makeAssertion label maxS prop = H.it label $ QC.withMaxSuccess maxS prop

assertConversion :: H.SpecWith ()
assertConversion =
  makeAssertion
    "conversion is consistent: DList.toList . DList.fromList == id"
    10000
    (conversion :: [Int] -> Bool)

assertConcatDList :: H.SpecWith ()
assertConcatDList =
  makeAssertion
    "concatenation is consistent"
    10000
    (concatDLists :: [Int] -> [Int] -> Bool)

assertConsDList :: H.SpecWith ()
assertConsDList =
  makeAssertion
    "cons is consistent"
    10000
    (consDLists :: Int -> [Int] -> Bool)
