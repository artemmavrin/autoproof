-- |
-- Module      : AutoProof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Propositional, intuitionistic natural deduction automated theorem-proving
-- library.
module AutoProof
  ( -- * Formulas
    Formula,

    -- ** Formula constructors
    lit,
    true,
    false,
    var,
    not,
    imp,
    or,
    and,
    iff,

    -- ** Infix formula constructors
    (-->),
    (<->),
    (\/),
    (/\),

    -- ** Operations on formulas
    subformulas,
    substitute,

    -- * Judgements
    Context,
    Judgement (Judgement, antecedents, consequent),

    -- ** Judgement constructor
    (|-),

    -- ** Operations on judgements
    weakenJudgement,

    -- * Proofs
    Proof,

    -- ** Proof constructors
    axiom,
    falseElim,
    impElim,
    impIntr,

    -- ** Provability testing and proof search
    isTautology,
    proveImp,
    toImp,

    -- ** Operations on proofs
    strengthenProof,
    weakenProof,

    -- ** Cuts
    findCut,
    hasCut,

    -- ** Proof correctness
    correct,
    valid,
    debug,

    -- * Parsing
    parseFormula,
    parseJudgement,

    -- * Pretty-printing
    prettyFormula,
    prettyJudgement,
    prettyProof,
  )
where

import AutoProof.Formula
  ( Formula,
    and,
    false,
    iff,
    imp,
    lit,
    not,
    or,
    prettyFormula,
    subformulas,
    substitute,
    true,
    var,
    (-->),
    (/\),
    (<->),
    (\/),
  )
import AutoProof.Judgement
  ( Context,
    Judgement (Judgement, antecedents, consequent),
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
    axiom,
    correct,
    debug,
    falseElim,
    findCut,
    hasCut,
    impElim,
    impIntr,
    isTautology,
    prettyProof,
    proveImp,
    strengthenProof,
    toImp,
    valid,
    weakenProof,
  )
import Prelude hiding (and, not, or)
