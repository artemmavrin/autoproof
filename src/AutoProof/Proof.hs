-- |
-- Module      : AutoProof.Proof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic proofs in propositional logic.
module AutoProof.Proof
  ( -- * Proof type
    Proof
      ( Axiom,
        FalseElim,
        TrueIntr,
        NotElim,
        NotIntr,
        ImpElim,
        ImpIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        AndElimL,
        AndElimR,
        AndIntr,
        IffElimL,
        IffElimR,
        IffIntr
      ),

    -- * Operations on proofs
    weakenProof,
    strengthenProof,

    -- * Proof search for judgements and formulas
    prove,
    proveTautology,

    -- * Specific proof search algorithms
    proveImp,
    proveStatman,

    -- * Provability checking
    isValid,
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
import AutoProof.Proof.Search
  ( isTautology,
    isValid,
    prove,
    proveImp,
    proveStatman,
    proveTautology,
  )
import AutoProof.Proof.Transform (strengthenProof, weakenProof)
import AutoProof.Proof.Types
  ( Proof
      ( AndElimL,
        AndElimR,
        AndIntr,
        Axiom,
        FalseElim,
        IffElimL,
        IffElimR,
        IffIntr,
        ImpElim,
        ImpIntr,
        NotElim,
        NotIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        TrueIntr
      ),
    prettyProof,
  )
