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
  ( -- * Types
    Formula (Lit, Var, Not, Imp, Or, And),
    Context,
    Sequent,
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

    -- * Constructors
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

    -- * Parsing
    parseFormula,
    parseSequent,

    -- * Proof debugging
    debug,
    valid,
  )
where

import Data.Prop.Parser
import Data.Prop.Proof.Debug
import Data.Prop.Proof.Types
import Data.Prop.Types
import Prelude hiding (and, not, or)
