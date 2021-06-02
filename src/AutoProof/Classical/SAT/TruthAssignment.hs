{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : AutoProof.Classical.SAT.TruthAssignment
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the TruthAssignment class.
module AutoProof.Classical.SAT.TruthAssignment
  ( TruthAssignment (evalVar, evalFormula, (|=)),
  )
where

import AutoProof.Internal.Formula
  ( Formula
      ( And,
        Iff,
        Imp,
        Lit,
        Not,
        Or,
        Var
      ),
  )
import Data.Map (Map)
import qualified Data.Map as Map

-- | Class representing truth assignments for evalutating propositional logic
-- formulas.
class TruthAssignment t a where
  -- | Evaluate the truth value of a variable under a given truth assignment.
  evalVar :: t -> a -> Bool

  -- | Evaluate the truth value of a formula under a given truth assignment.
  evalFormula :: t -> Formula a -> Bool
  evalFormula _ (Lit b) = b
  evalFormula t (Var x) = evalVar t x
  evalFormula t (Not a) = not (evalFormula t a)
  evalFormula t (Imp a b) = not (evalFormula t a) || evalFormula t b
  evalFormula t (Or a b) = evalFormula t a || evalFormula t b
  evalFormula t (And a b) = evalFormula t a && evalFormula t b
  evalFormula t (Iff a b) = evalFormula t (And (Imp a b) (Imp b a))

  -- | Infix alias of 'evalFormula'.
  --
  -- ==== __Examples__
  --
  -- >>> t = Map.fromList $ [("a", True), ("b", False)]
  -- >>> t |= And (Var "a") (Not (Var "b"))
  -- True
  (|=) :: t -> Formula a -> Bool
  (|=) = evalFormula

-- | Truth assignments where a missing key in the map is interpreted as false.
--
-- ==== __Examples__
--
-- >>> t = Map.fromList $ [("a", True), ("b", False)]
-- >>> t |= And (Var "a") (Not (Var "b"))
-- True
--
-- >>> t = Map.fromList $ [("a", True)]
-- >>> t |= Imp (Var "a") (Var "b")
-- False
instance Ord a => TruthAssignment (Map a Bool) a where
  evalVar t x = Map.findWithDefault False x t

-- | Can be used to evaluate closed formulas
--
-- ==== __Examples__
--
-- >>> () |= (Var "a")
-- False
--
-- >>> () |= (Or (Lit True) (Lit False))
-- True
instance TruthAssignment () a where
  evalVar () = const False
