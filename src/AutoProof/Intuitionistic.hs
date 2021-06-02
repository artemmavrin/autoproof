-- |
-- Module      : AutoProof.Intuitionistic
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic propositional logic library.
module AutoProof.Intuitionistic
  ( -- * Formulas
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- ** Operations on formulas
    subformulas,
    substitute,

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
    isProvable,
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

import AutoProof.Internal.AST
  ( AST (Root, children, height, root, size),
    properSubtrees,
    subtrees,
  )
import AutoProof.Internal.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    prettyFormula,
    subformulas,
    substitute,
  )
import AutoProof.Internal.Judgement
  ( Context,
    Judgement (Judgement, antecedents, succedent),
    prettyJudgement,
    weakenJudgement,
    (|-),
  )
import AutoProof.Internal.Parser
  ( parseFormula,
    parseJudgement,
  )
import AutoProof.Internal.Proof
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
import AutoProof.Internal.Utils.PrettyPrintable (PrettyPrintable (pretty))
import AutoProof.Intuitionistic.Proof
  ( correct,
    debug,
    findCut,
    hasCut,
    isProvable,
    isTautology,
    prove,
    proveImp,
    proveStatman,
    proveTautology,
    valid,
  )
