-- |
-- Module      : Data.Prop.Proof
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic natural deduction proofs.
module Data.Prop.Proof
  ( -- * The @Proof@ type
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

    -- * Proofs in the implicational fragment
    proveImp,

    -- * Proof debugging
    debug,
    valid,
  )
where

import Data.Prop.Proof.Debug
import Data.Prop.Proof.Implication
import Data.Prop.Proof.Types
