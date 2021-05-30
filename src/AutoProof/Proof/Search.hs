-- |
-- Module      : AutoProof.Proof.Search
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Proof search algorithms.
module AutoProof.Proof.Search
  ( -- * Proof search for judgements and formulas
    prove,
    proveTautology,

    -- * Specific proof search algorithms
    proveStatman,
    proveImp,

    -- * Provability checking
    isValid,
    isTautology,
  )
where

import AutoProof.Proof.Search.General
  ( isTautology,
    isValid,
    prove,
    proveTautology,
  )
import AutoProof.Proof.Search.Implication (proveImp)
import AutoProof.Proof.Search.Statman (proveStatman)
