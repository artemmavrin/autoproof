-- |
-- Module      : AutoProof.Intuitionistic.Proof.Provability
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Provability checking.
module AutoProof.Intuitionistic.Proof.Provability
  ( isProvable,
    isTautology,
  )
where

import AutoProof.Internal.Formula (Formula)
import AutoProof.Internal.Judgement (Judgement, (|-))
import AutoProof.Intuitionistic.Proof.Search (prove)
import Data.Maybe (isJust)

-- | Determine whether a judgement is intuitionistically valid.
--
-- ==== __Examples__
--
-- >>> isProvable $ [Var "a", Var "b"] |- And (Var "a") (Var "b")
-- True
--
-- >>> isProvable $ [] |- Or (Var "a") (Not (Var "a"))
-- False
isProvable :: Ord a => Judgement a -> Bool
isProvable = isJust . prove

-- | Determine whether a formula is an intuitionistic tautology.
--
-- ==== __Examples__
--
-- >>> isTautology $ Imp (And (Var 'a') (Var 'b')) (Var 'a')
-- True
--
-- >>> isTautology $ Or (Var 'a') (Not (Var 'a'))
-- False
--
-- >>> isTautology $ Not (Not (Or (Var 'a') (Not (Var 'a'))))
-- True
isTautology :: Ord a => Formula a -> Bool
isTautology = isProvable . ([] |-)
