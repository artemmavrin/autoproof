{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AutoProof.Formula
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Formula' type and related functions.
module AutoProof.Formula
  ( -- * Formula type
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- * Pretty-printing
    prettyFormula,

    -- * Operations on formulas
    substitute,
    subformulas,
    atoms,
  )
where

import AutoProof.Formula.Operations
  ( atoms,
    subformulas,
    substitute,
  )
import AutoProof.Formula.Types
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    prettyFormula,
  )
