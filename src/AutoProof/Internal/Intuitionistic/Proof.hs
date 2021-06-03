-- |
-- Module      : AutoProof.Internal.Intuitionistic.Proof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic proofs in propositional logic.
module AutoProof.Internal.Intuitionistic.Proof
  ( -- * Proof search for judgements and formulas
    prove,
    proveTautology,

    -- * Specific proof search algorithms
    proveImp,
    proveStatman,

    -- * Provability checking
    isProvable,
    isTautology,

    -- * Cuts
    findCut,
    hasCut,

    -- * Proof correctness
    correct,
    valid,
    debug,
  )
where

import AutoProof.Internal.Intuitionistic.Proof.Correctness
  ( correct,
    debug,
    valid,
  )
import AutoProof.Internal.Intuitionistic.Proof.Cut
  ( findCut,
    hasCut,
  )
import AutoProof.Internal.Intuitionistic.Proof.Provability
  ( isProvable,
    isTautology,
  )
import AutoProof.Internal.Intuitionistic.Proof.Search
  ( prove,
    proveImp,
    proveStatman,
    proveTautology,
  )
