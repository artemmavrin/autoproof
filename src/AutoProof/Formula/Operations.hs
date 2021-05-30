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
  ( subformulas,
  )
where

import AutoProof.Formula.Types (Formula (And, Iff, Imp, Lit, Not, Or, Var))
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
