-- |
-- Module      : AutoProof.Internal.Formula.Operations
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines miscellaneous operations on formulas.
module AutoProof.Internal.Formula.Operations
  ( subformulas,
    substitute,
  )
where

import AutoProof.Internal.Formula.Types
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
  )
import Data.Set (Set)
import qualified Data.Set as Set

-- | Get the set of subformulas of a propositional formula.
--
-- ==== __Examples__
--
-- >>> subformulas $ Or (Var 'x') (And (Var 'y') (Var 'z'))
-- fromList [Var 'x',Var 'y',Var 'z',And (Var 'y') (Var 'z'),Or (Var 'x') (And (Var 'y') (Var 'z'))]
subformulas :: Ord a => Formula a -> Set (Formula a)
subformulas = go Set.empty
  where
    go s p@(Lit _) = Set.insert p s
    go s p@(Var _) = Set.insert p s
    go s p@(Not a) = go (Set.insert p s) a
    go s p@(Imp a b) = go (go (Set.insert p s) a) b
    go s p@(Or a b) = go (go (Set.insert p s) a) b
    go s p@(And a b) = go (go (Set.insert p s) a) b
    go s p@(Iff a b) = go (go (Set.insert p s) a) b

-- | @('substitute' a x p)@ represents \(a[x := p]\), the substitution of each
-- occurence of the variable \(x\) in the formula \(a\) by the formula \(p\).
--
-- ==== __Examples__
--
-- >>> substitute (Imp (Var 'e') (Var 'e')) 'e' (And (Var 'a') (Var 'a'))
-- Imp (And (Var 'a') (Var 'a')) (And (Var 'a') (Var 'a'))
substitute :: Eq a => Formula a -> a -> Formula a -> Formula a
substitute a@(Lit _) _ _ = a
substitute v@(Var y) x p = if x == y then p else v
substitute (Not a) x p = Not $ substitute a x p
substitute (Imp a b) x p = Imp (substitute a x p) (substitute b x p)
substitute (Or a b) x p = Or (substitute a x p) (substitute b x p)
substitute (And a b) x p = And (substitute a x p) (substitute b x p)
substitute (Iff a b) x p = Iff (substitute a x p) (substitute b x p)
