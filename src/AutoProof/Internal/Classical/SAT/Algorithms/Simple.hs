-- |
-- Module      : AutoProof.Internal.Classical.SAT.Algorithms.Simple
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A simple baseline SAT algorithm.
module AutoProof.Internal.Classical.SAT.Algorithms.Simple
  ( simpleSAT,
    simpleSATAssignment,
  )
where

import AutoProof.Internal.Classical.SAT.TruthAssignment
  ( TruthAssignment (evalFormula),
  )
import AutoProof.Internal.Formula (Formula (Lit), getAnyVariable, substitute)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map

-- | A simple baseline satisfiability algorithm.
--
-- This algorithm is based on the observation that if \(a\) is a propositional
-- formula and \(x\) is a propositional variable, then \(a\) is satisfiable if
-- and only if either \(a[\top/x]\) is satisfiable or \(a[\bot/x]\) is
-- satisfiable.
--
-- ==== __Examples__
--
-- >>> simpleSAT $ Var "a"
-- True
--
-- >>> simpleSAT $ And (Var "a") (Not (Var "a"))
-- False
simpleSAT :: Eq a => Formula a -> Bool
simpleSAT a = case getAnyVariable a of
  Nothing -> evalFormula () a
  Just x ->
    simpleSAT (substitute a x (Lit True))
      || simpleSAT (substitute a x (Lit False))

-- | A simple baseline satisfiability algorithm, returning a satisfying truth
-- assignment if there is one.
--
-- This algorithm is based on the observation that if \(a\) is a propositional
-- formula and \(x\) is a propositional variable, then \(a\) is satisfiable if
-- and only if either \(a[\top/x]\) is satisfiable or \(a[\bot/x]\) is
-- satisfiable.
--
-- ==== __Examples__
--
--
-- >>> a = And (Not (Var "a")) (Or (Var "b") (Var "c")) -- satisfiable
-- >>> Just t = simpleSATAssignment a
-- >>> t
-- fromList [("a",False),("b",True),("c",True)]
-- >>> t |= a
-- True
--
-- >>> simpleSATAssignment $ And (Var "a") (Not (Var "a")) -- unsatisfiable
-- Nothing
simpleSATAssignment :: Ord a => Formula a -> Maybe (Map a Bool)
simpleSATAssignment = f Map.empty
  where
    f m a = case getAnyVariable a of
      Nothing -> if evalFormula () a then Just m else Nothing
      Just x ->
        f (Map.insert x True m) (substitute a x (Lit True))
          <|> f (Map.insert x False m) (substitute a x (Lit False))
