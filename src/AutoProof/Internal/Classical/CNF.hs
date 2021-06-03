-- |
-- Module      : AutoProof.Internal.Classical.CNF
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions and types related to conjunctive normal forms.
module AutoProof.Internal.Classical.CNF
  ( -- * Types
    CNF,
    Clause,
    Literal,

    -- * Conversion functions
    fromFormula,
    toFormula,
    canonicalCNF,

    -- * Operations
    substitute,
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
import Data.Set (Set)
import qualified Data.Set as Set

-- | Represents either a propositional variable or its negation, depending on
-- whether the boolean first component is 'True' or 'False', respectively.
type Literal a = (Bool, a)

-- | Represents a disjunction of literals.
type Clause a = Set (Literal a)

-- | Conjunctive normal form: represents a conjunction of clauses.
type CNF a = Set (Clause a)

-- | Convert a 'Formula' into conjunctive normal form.
--
-- Adapted from Figure 2.6 in
--
-- *  Samuel Mimram (2020)
--    /PROGRAM = PROOF/.
fromFormula :: Ord a => Formula a -> CNF a
fromFormula = f . pos
  where
    merge a b = foldr Set.union Set.empty (Set.map (flip Set.map a . Set.union) b)

    pos (Lit True) = Set.empty
    pos (Lit False) = Set.singleton Set.empty
    pos (Var x) = Set.singleton (Set.singleton (True, x))
    pos (Not a) = neg a
    pos (Imp a b) = merge (neg a) (pos b)
    pos (Or a b) = merge (pos a) (pos b)
    pos (And a b) = Set.union (pos a) (pos b)
    pos (Iff a b) = pos (And (Imp a b) (Imp b a))

    neg (Lit True) = Set.singleton Set.empty
    neg (Lit False) = Set.empty
    neg (Var x) = Set.singleton (Set.singleton (False, x))
    neg (Not a) = pos a
    neg (Imp a b) = Set.union (pos a) (neg b)
    neg (Or a b) = Set.union (neg a) (neg b)
    neg (And a b) = merge (neg a) (neg b)
    neg (Iff a b) = neg (And (Imp a b) (Imp b a))

    -- Remove redundant clauses from a CNF formula
    f = Set.filter g

    -- Return whether a clause contains a literal and its negation
    g c = not (any (h c) c)

    -- Test whether a literal and its negation both occur in a clause
    h c (b, x) = (not b, x) `Set.member` c

-- | Convert a conjunctive normal form representation of a formula into a
-- formula.
toFormula :: CNF a -> Formula a
toFormula clauses =
  if any Set.null clauses
    then Lit False
    else case Set.toList clauses of
      [] -> Lit True
      (c : cs) -> foldr (And . clauseToFormula) (clauseToFormula c) cs

clauseToFormula :: Clause a -> Formula a
clauseToFormula literals = case Set.toList literals of
  [] -> Lit False
  (l : ls) -> foldr (Or . literalToFormula) (literalToFormula l) ls

literalToFormula :: Literal a -> Formula a
literalToFormula (False, x) = Not (Var x)
literalToFormula (True, x) = Var x

-- | Convert a formula into a canonical conjunctive normal form.
--
-- ==== __Examples__
--
-- >>> canonicalCNF $ Or (Not (Imp (Var "a") (Var "b"))) (Var "c")
-- And (Or (Var "c") (Var "a")) (Or (Var "c") (Not (Var "b")))
canonicalCNF :: Ord a => Formula a -> Formula a
canonicalCNF = toFormula . fromFormula

-- | Substitute a boolean value for a variable in a CNF formula.
--
-- @('substitute' a x 'True')@ and @('substitute' a x 'False')@ represent the
-- substitutions \(a[\top/x]\) and \(a[\bot/x]\), respectively.
--
-- ==== __Examples__
--
-- >>> a = And (Imp (Var "a") (Not (And (Var "b") (Not (Var "c"))))) (Var "a")
-- >>> cnf = CNF.fromFormula a
-- >>> cnf
-- fromList [fromList [(False,"a"),(False,"b"),(True,"c")],fromList [(True,"a")]]
-- >>> cnf2 = CNF.substitute cnf "a" True
-- >>> cnf2
-- fromList [fromList [(False,"b"),(True,"c")]]
-- >>> a2 = CNF.toFormula cnf2
-- >>> a2
-- Or (Var "c") (Not (Var "b"))
substitute :: Ord a => CNF a -> a -> Bool -> CNF a
substitute a x b =
  Set.map (Set.filter (/= (not b, x))) (Set.filter (not . Set.member (b, x)) a)
