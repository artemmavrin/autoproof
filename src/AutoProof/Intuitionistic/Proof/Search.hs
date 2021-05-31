-- |
-- Module      : AutoProof.Intuitionistic.Proof.Search
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Proof search algorithms.
module AutoProof.Intuitionistic.Proof.Search
  ( -- * Proof search for judgements and formulas
    prove,
    proveTautology,

    -- * Specific proof search algorithms
    proveStatman,
    proveImp,
  )
where

import AutoProof.Intuitionistic.Proof.Search.General
  ( prove,
    proveTautology,
  )
import AutoProof.Intuitionistic.Proof.Search.Implication (proveImp)
import AutoProof.Intuitionistic.Proof.Search.Statman (proveStatman)
