-- |
-- Module      : AutoProof.Proof.Search.General
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A general-purpose proof search algorithm.
module AutoProof.Proof.Search.General
  ( -- * Proof search for judgements and formulas
    prove,
    proveTautology,

    -- * Provability checking
    isValid,
    isTautology,
  )
where

import AutoProof.Formula (Formula)
import AutoProof.Judgement (Judgement, (|-))
import AutoProof.Proof.Search.Implication (proveImp)
import AutoProof.Proof.Search.Statman (proveStatman)
import AutoProof.Proof.Types (Proof)
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (isJust)

-- | Find an intuitionistic proof of a judgement, if a proof exists.
prove :: Ord a => Judgement a -> Maybe (Proof a)
prove j =
  -- Try proving the judgement as an implicational judgement first (this can
  -- fail if not every formula ocurring in j is implicational).
  proveImp j
    -- Fall back to Statman's algorithm for non-implicational judgements.
    <|> proveStatman j

-- | Find an intuitionistic proof of a formula, if a proof exists.
proveTautology :: Ord a => Formula a -> Maybe (Proof a)
proveTautology = prove . ([] |-)

-- | Determine whether a judgement is intuitionistically valid.
--
-- ==== __Examples__
--
-- >>> isValid $ [Var "a", Var "b"] |- And (Var "a") (Var "b")
-- True
--
-- >>> isValid $ [] |- Or (Var "a") (Not (Var "a"))
-- False
isValid :: Ord a => Judgement a -> Bool
isValid = isJust . prove

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
isTautology = isJust . prove . ([] |-)
