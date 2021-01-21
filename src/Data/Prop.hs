-- |
-- Module      : Data.Prop
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic propositional logic.
module Data.Prop
  ( -- * Types for propositional formulas and proofs
    Formula (Lit, Var, Not, Imp, Or, And),
    Context,
    Proof
      ( Ax,
        TopIntr,
        BotElim,
        NotElim,
        NotIntr,
        ImpElim,
        ImpIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        AndElimL,
        AndElimR,
        AndIntr
      ),

    -- * Propositional formula constructors
    true,
    false,
    lit,
    var,
    not,
    imp,
    implies,
    or,
    and,
    iff,
    (-->),
    (\/),
    (/\),
    (<->),

    -- * Proof debugging
    debug,
    valid,
  )
where

import Data.Prop.Proof.Debug
import Data.Prop.Proof.Types
import Data.Prop.Types
import Prelude hiding (and, not, or)
