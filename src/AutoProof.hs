-- |
-- Module      : AutoProof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Propositional logic library.
--
-- This top-level module only includes type definitions and functions for the
-- syntactic entities of propositional logic. If you want semantics, then you
-- must first choose either classical or intuitionistic logic, and then use the
-- corresponding module
--
--  1.  "AutoProof.Classical", for classical logic, or
--  2.  "AutoProof.Intuitionistic", for intuitionistic logic.
--
-- Both of the above module expose every symbol in this module, and more.
module AutoProof
  ( -- * Formulas
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- ** Operations on formulas
    subformulas,
    substitute,
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
