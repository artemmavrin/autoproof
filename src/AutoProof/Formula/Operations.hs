-- |
-- Module      : AutoProof.Formula.Operations
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines miscellaneous operations on formulas.
module AutoProof.Formula.Operations
  ( substitute,
    subformulas,
    atoms,
  )
where

import AutoProof.Formula.Types
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    and,
    iff,
    imp,
    not,
    or,
  )
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (and, not, or)

-- | Get the set of subformulas of a propositional formula.
--
-- ==== __Examples__
--
-- >>> subformulas $ or (var 'x') (and (var 'y') (var 'z'))
-- fromList [var 'x',var 'y',var 'z',and (var 'y') (var 'z'),or (var 'x') (and (var 'y') (var 'z'))]
subformulas :: Ord a => Formula a -> Set (Formula a)
subformulas = go Set.empty
  where
    go s p@(Lit _ _) = Set.insert p s
    go s p@(Var _ _) = Set.insert p s
    go s p@(Not _ a) = go (Set.insert p s) a
    go s p@(Imp _ a b) = go (go (Set.insert p s) a) b
    go s p@(Or _ a b) = go (go (Set.insert p s) a) b
    go s p@(And _ a b) = go (go (Set.insert p s) a) b
    go s p@(Iff _ a b) = go (go (Set.insert p s) a) b

-- | @('substitute' a x p)@ represents \(a[x := p]\), the substitution of each
-- occurence of the variable \(x\) in the formula \(a\) by the formula \(p\).
--
-- ==== __Examples__
--
-- >>> substitute (var 'e' --> var 'e') 'e' (var 'a' /\ var 'a')
-- imp (and (var 'a') (var 'a')) (and (var 'a') (var 'a'))
substitute :: Eq a => Formula a -> a -> Formula a -> Formula a
substitute a@(Lit _ _) _ _ = a
substitute v@(Var _ y) x p = if x == y then p else v
substitute (Not _ a) x p = not $ substitute a x p
substitute (Imp _ a b) x p = imp (substitute a x p) (substitute b x p)
substitute (Or _ a b) x p = or (substitute a x p) (substitute b x p)
substitute (And _ a b) x p = and (substitute a x p) (substitute b x p)
substitute (Iff _ a b) x p = iff (substitute a x p) (substitute b x p)

-- | @('atoms' a)@ is the set of atoms (propositional variables/symbols) that
-- occur in the formula \(a\).
--
-- ==== __Examples__
--
-- >>> atoms (var "a" \/ var "b" --> var "c" /\ var "d")
-- fromList ["a","b","c","d"]
atoms :: Ord a => Formula a -> Set a
atoms = go Set.empty
  where
    go s (Lit _ _) = s
    go s (Var _ x) = Set.insert x s
    go s (Not _ a) = go s a
    go s (Imp _ a b) = go (go s a) b
    go s (Or _ a b) = go (go s a) b
    go s (And _ a b) = go (go s a) b
    go s (Iff _ a b) = go (go s a) b
