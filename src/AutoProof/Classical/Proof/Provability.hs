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
-- >>> isProvable $ [Not (Not (Var 'a'))] |- Var 'a'
-- True
--
-- >>> isProvable $ [Var 'a'] |- Var 'b'
-- False
isProvable :: Ord a => Judgement a -> Bool
isProvable = isProvableGlivenko

-- | Determine whether a formula is a classically tautology.
--
-- ==== __Examples__
--
-- >>> isTautology $ Or (Var 'a') (Not (Var 'a'))
-- True
-- >>> isTautology $ Var 'a'
-- False
isTautology :: Ord a => Formula a -> Bool
isTautology = isProvable . ([] |-)
