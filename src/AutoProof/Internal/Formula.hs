-- |
-- Module      : AutoProof.Internal.Formula
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Formula' type and related functions.
module AutoProof.Internal.Formula
  ( -- * Formula type
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- * Pretty-printing
    prettyFormula,

    -- * Operations on formulas
    subformulas,
    substitute,
    getAnyVariable,
  )
where

import AutoProof.Internal.Formula.Operations
  ( getAnyVariable,
    subformulas,
    substitute,
  )
import AutoProof.Internal.Formula.Types
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    prettyFormula,
  )
