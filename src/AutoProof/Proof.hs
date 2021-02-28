-- |
-- Module      : AutoProof.Proof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic natural deductions in propositional logic.
module AutoProof.Proof
  ( -- * Proof type
    Proof (Ax, ImpElim, ImpIntr),

    -- ** Proof constructors
    axiom,
    impElim,
    impIntr,

    -- * Proof metadata
    height,
    judgement,
    premises,

    -- * Operations on proofs
    weakenProof,
    strengthenProof,

    -- * Proof search and provability checking
    proveImp,
    toImp,
    isTautology,

    -- * Cuts
    findCut,
    hasCut,

    -- * Pretty-printing
    prettyProof,

    -- * Proof correctness
    correct,
    valid,
    debug,
  )
where

import AutoProof.Proof.Correctness (correct, debug, valid)
import AutoProof.Proof.Cut (findCut, hasCut)
import AutoProof.Proof.Implication (proveImp)
import AutoProof.Proof.Provability (isTautology, toImp)
import AutoProof.Proof.Transform (strengthenProof, weakenProof)
import AutoProof.Proof.Types
  ( Proof (Ax, ImpElim, ImpIntr),
    axiom,
    height,
    impElim,
    impIntr,
    judgement,
    premises,
    prettyProof,
  )
