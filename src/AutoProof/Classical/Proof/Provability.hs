-- |
-- Module      : AutoProof.Classical.Proof.Provablity
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Provability checking in classical propositional logic.
module AutoProof.Classical.Proof.Provability
  ( -- * Provability checking
    isProvable,
    isTautology,
  )
where

import AutoProof.Classical.Proof.Glivenko (isProvableGlivenko)
import AutoProof.Internal.Formula (Formula)
import AutoProof.Internal.Judgement (Judgement, (|-))

-- | Determine whether a judgement is classically valid.
--
-- ==== __Examples__
--
-- >>> isProvable $ [Var "a", Var "b"] |- And (Var "a") (Var "b")
-- True
--
-- >>> isProvable $ [] |- Or (Var "a") (Not (Var "a"))
-- False
isProvable :: Ord a => Judgement a -> Bool
isProvable = isProvableGlivenko

-- | Determine whether a formula is an classically tautology.
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
