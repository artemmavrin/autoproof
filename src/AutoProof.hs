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

    -- * Judgements
    Context,
    Judgement (Judgement, antecedents, succedent),

    -- ** Judgement constructor
    (|-),

    -- ** Operations on judgements
    weakenJudgement,

    -- * Proofs
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

    -- ** Proof search for judgements and formulas
    prove,
    proveTautology,

    -- ** Specific proof search algorithms
    proveStatman,
    proveImp,

    -- ** Provability checking
    isValid,
    isTautology,

    -- ** Cuts
    findCut,
    hasCut,

    -- ** Proof correctness
    correct,
    valid,
    debug,

    -- * Abstract syntax trees
    AST (Root, root, children, height, size),
    subtrees,
    properSubtrees,

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

import AutoProof.AST
  ( AST (Root, children, height, root, size),
    properSubtrees,
    subtrees,
  )
import AutoProof.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    prettyFormula,
    subformulas,
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
    correct,
    debug,
    findCut,
    hasCut,
    isTautology,
    isValid,
    prettyProof,
    prove,
    proveImp,
    proveStatman,
    proveTautology,
    valid,
  )
import AutoProof.Utils.PrettyPrintable (PrettyPrintable (pretty))
