-- |
-- Module      : AutoProof.Classical.CNF
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions and types related to conjunctive normal forms.
--
-- This module is intended to be imported with a qualified name, as in
--
-- > import qualified AutoProof.Classical.CNF as CNF
--
-- This gives the conversion functions in this module descriptive names like
-- @CNF.'fromFormula'@ and @CNF.'toFormula'@.
module AutoProof.Classical.CNF
  ( -- * Types
    CNF,
    Clause,
    Literal,

    -- * Conversion functions
    fromFormula,
    toFormula,
    canonicalCNF,

    -- * Operations
    substitute,
    unitLiteral,
    pureLiteral,
  )
where

import AutoProof.Internal.Classical.CNF
  ( CNF,
    Clause,
    Literal,
    canonicalCNF,
    fromFormula,
    pureLiteral,
    substitute,
    toFormula,
    unitLiteral,
  )
