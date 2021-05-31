-- |
-- Module      : AutoProof.Intuitionistic.Proof.Search.General
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A general-purpose proof search algorithm.
module AutoProof.Intuitionistic.Proof.Search.General
  ( -- * Proof search for judgements and formulas
    prove,
    proveTautology,
  )
where

import AutoProof.Internal.Formula (Formula)
import AutoProof.Internal.Judgement (Judgement, (|-))
import AutoProof.Internal.Proof.Types (Proof)
import AutoProof.Intuitionistic.Proof.Search.Implication (proveImp)
import AutoProof.Intuitionistic.Proof.Search.Statman (proveStatman)
import Control.Applicative (Alternative ((<|>)))

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
