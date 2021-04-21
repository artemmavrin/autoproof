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
      ( Ax,
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

    -- ** Proof constructors
    axiom,
    falseElim,
    trueIntr,
    notElim,
    notIntr,
    impElim,
    impIntr,
    orElim,
    orIntrL,
    orIntrR,
    andElimL,
    andElimR,
    andIntr,
    iffElimL,
    iffElimR,
    iffIntr,

    -- * Operations on proofs
    weakenProof,
    strengthenProof,

    -- * Proof search and provability checking
    proveImp,
    toImp,
    isTautology,
    proveTautology,

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
import AutoProof.Proof.Provability (isTautology, proveTautology, toImp)
import AutoProof.Proof.Transform (strengthenProof, weakenProof)
import AutoProof.Proof.Types
  ( Proof
      ( AndElimL,
        AndElimR,
        AndIntr,
        Ax,
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
    andElimL,
    andElimR,
    andIntr,
    axiom,
    falseElim,
    iffElimL,
    iffElimR,
    iffIntr,
    impElim,
    impIntr,
    notElim,
    notIntr,
    orElim,
    orIntrL,
    orIntrR,
    prettyProof,
    trueIntr,
  )
