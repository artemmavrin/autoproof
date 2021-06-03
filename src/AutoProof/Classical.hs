-- |
-- Module      : AutoProof.Classical
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Classical propositional logic library.
module AutoProof.Classical
  ( -- * Formulas
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- ** Operations on formulas
    subformulas,
    substitute,
    canonicalCNF,
    getAnyVariable,

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

    -- ** Provability checking
    isProvable,
    isTautology,

    -- * Satisfiability

    -- ** Truth assignments
    TruthAssignment (evalVar, evalFormula),
    (|=),

    -- ** Check satisfiability
    satSimple,
    satDPLL,

    -- ** Satisfying assignment search
    satAssignmentSimple,
    satAssignmentDPLL,

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
import AutoProof.Internal.Classical.CNF (canonicalCNF)
import AutoProof.Internal.Classical.Proof
  ( isProvable,
    isTautology,
  )
import AutoProof.Internal.Classical.SAT
  ( TruthAssignment (evalFormula, evalVar),
    satAssignmentDPLL,
    satAssignmentSimple,
    satDPLL,
    satSimple,
    (|=),
  )
import AutoProof.Internal.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    getAnyVariable,
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
