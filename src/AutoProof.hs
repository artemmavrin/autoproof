-- |
-- Module      : AutoProof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Automated theorem-proving in intuitionistic propositional logic.
module AutoProof
  ( -- * Formulas
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- ** Operations on formulas
    subformulas,
    substitute,
    atoms,

    -- * Judgements
    Context,
    Judgement (Judgement, antecedents, succedent),

    -- ** Judgement constructor
    (|-),

    -- ** Operations on judgements
    weakenJudgement,

    -- * Proofs
    Proof,

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

    -- ** Provability testing and proof search
    isTautology,
    proveTautology,
    proveImp,
    toImp,

    -- ** Cuts
    findCut,
    hasCut,

    -- ** Proof correctness
    correct,
    valid,
    debug,

    -- * Abstract syntax trees
    AST (Root, root, children, height, size),

    -- * Parsing
    parseFormula,
    parseJudgement,

    -- * Pretty-printing
    pretty,
    prettyFormula,
    prettyJudgement,
    prettyProof,
  )
where

import AutoProof.AST (AST (Root, children, height, root, size))
import AutoProof.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    atoms,
    prettyFormula,
    subformulas,
    substitute,
  )
import AutoProof.Judgement
  ( Context,
    Judgement (Judgement, antecedents, succedent),
    prettyJudgement,
    weakenJudgement,
    (|-),
  )
import AutoProof.Parser
  ( parseFormula,
    parseJudgement,
  )
import AutoProof.Proof
  ( Proof,
    andElimL,
    andElimR,
    andIntr,
    axiom,
    correct,
    debug,
    falseElim,
    findCut,
    hasCut,
    iffElimL,
    iffElimR,
    iffIntr,
    impElim,
    impIntr,
    isTautology,
    notElim,
    notIntr,
    orElim,
    orIntrL,
    orIntrR,
    prettyProof,
    proveImp,
    proveTautology,
    toImp,
    trueIntr,
    valid,
  )
import AutoProof.Utils.PrettyPrintable (PrettyPrintable (pretty))
