-- |
-- Module      : AutoProof.Classical.CNF
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions and types related to conjunctive normal forms.
module AutoProof.Classical.CNF
  ( -- * Types
    CNF,
    Clause,
    Literal,

    -- * Conversion functions
    fromFormula,
    toFormula,
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

-- | Either a propositional variable or its negation, depending on whether
-- the first component is 'True' or 'False', respectively.
type Literal a = (Bool, a)

-- | A disjunction of literals.
type Clause a = [Literal a]

-- | Conjunctive normal form: a conjunction of literals.
type CNF a = [Clause a]

-- | Convert a 'Formula' into conjunctive normal form.
--
-- Adapted from Figure 2.6 in
--
-- *  Samuel Mimram (2020)
--    /PROGRAM = PROOF/.
fromFormula :: Formula a -> CNF a
fromFormula = pos
  where
    merge a b = concatMap (\c -> map (c ++) b) a

    pos (Lit True) = []
    pos (Lit False) = [[]]
    pos (Var x) = [[(True, x)]]
    pos (Not a) = neg a
    pos (Imp a b) = merge (neg a) (pos b)
    pos (Or a b) = merge (pos a) (pos b)
    pos (And a b) = pos a ++ pos b
    pos (Iff a b) = pos (And (Imp a b) (Imp b a))

    neg (Lit True) = [[]]
    neg (Lit False) = []
    neg (Var x) = [[(False, x)]]
    neg (Not a) = pos a
    neg (Imp a b) = pos a ++ neg b
    neg (Or a b) = neg a ++ neg b
    neg (And a b) = merge (neg a) (neg b)
    neg (Iff a b) = neg (And (Imp a b) (Imp b a))

-- | Convert a conjunctive normal form representation of a formula into a
-- formula.
toFormula :: CNF a -> Formula a
toFormula [] = Lit True
toFormula (c : cs) = foldr (And . clauseToFormula) (clauseToFormula c) cs

clauseToFormula :: Clause a -> Formula a
clauseToFormula [] = Lit False
clauseToFormula (l : ls) = foldr (Or . literalToFormula) (literalToFormula l) ls

literalToFormula :: Literal a -> Formula a
literalToFormula (False, x) = Not (Var x)
literalToFormula (True, x) = Var x
