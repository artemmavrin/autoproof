-- |
-- Module      : AutoProof.Intuitionistic.Proof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic proofs in propositional logic.
module AutoProof.Intuitionistic.Proof
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

import AutoProof.Intuitionistic.Proof.Correctness (correct, debug, valid)
import AutoProof.Intuitionistic.Proof.Cut (findCut, hasCut)
import AutoProof.Intuitionistic.Proof.Provability (isProvable, isTautology)
import AutoProof.Intuitionistic.Proof.Search
  ( prove,
    proveImp,
    proveStatman,
    proveTautology,
  )
