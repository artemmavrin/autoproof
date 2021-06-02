-- |
-- Module      : AutoProof.Classical.SAT.Baseline
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Naive baseline SAT algorithm.
module AutoProof.Classical.SAT.Baseline
  ( naiveSAT,
    naiveSATAssignment,
  )
where

import AutoProof.Classical.SAT.TruthAssignment (TruthAssignment (evalFormula))
import AutoProof.Internal.Formula (Formula (Lit), getAnyVariable, substitute)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map

-- | Naive satisfiability algorithm.
--
-- This algorithm is based on the observation that if \(a\) is a propositional
-- formula and \(x\) is a propositional variable, then \(a\) is satisfiable if
-- and only if either \(a[x := \top]\) is satisfiable or \(a[x := \bot]\) is
-- satisfiable.
--
-- ==== __Examples__
--
-- >>> naiveSAT $ Var "a"
-- True
--
-- >>> naiveSAT $ And (Var "a") (Not (Var "a"))
-- False
naiveSAT :: Eq a => Formula a -> Bool
naiveSAT a = case getAnyVariable a of
  Nothing -> evalFormula () a
  Just x ->
    naiveSAT (substitute a x (Lit True))
      || naiveSAT (substitute a x (Lit False))

-- | Naive satisfiability algorithm, returning a satisfying truth assignment.
--
-- This algorithm is based on the observation that if \(a\) is a propositional
-- formula and \(x\) is a propositional variable, then \(a\) is satisfiable if
-- and only if either \(a[x := \top]\) is satisfiable or \(a[x := \bot]\) is
-- satisfiable.
--
-- ==== __Examples__
--
-- >>> naiveSATAssignment $ And (Not (Var "a")) (Or (Var "b") (Var "c"))
-- Just (fromList [("a",False),("b",True),("c",True)])
--
-- >>> naiveSATAssignment $ And (Var "a") (Not (Var "a"))
-- Nothing
naiveSATAssignment :: Ord a => Formula a -> Maybe (Map a Bool)
naiveSATAssignment = f Map.empty
  where
    f m a = case getAnyVariable a of
      Nothing -> if evalFormula () a then Just m else Nothing
      Just x ->
        f (Map.insert x True m) (substitute a x (Lit True))
          <|> f (Map.insert x False m) (substitute a x (Lit False))
