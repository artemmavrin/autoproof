-- |
-- Module      : AutoProof.Proof.Cut
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions related to cuts in proofs.
module AutoProof.Proof.Cut (findCut, hasCut) where

import AutoProof.Proof.Types
  ( Proof
      ( AndElimL,
        AndElimR,
        AndIntr,
        IffElimL,
        IffElimR,
        IffIntr,
        ImpElim,
        ImpIntr,
        NotElim,
        NotIntr,
        OrElim,
        OrIntrL,
        OrIntrR
      ),
  )
import Data.Maybe (isJust)

-- | Find the cut nearest the root of a proof, if any.
findCut :: Proof a -> Maybe (Proof a)
findCut p@(ImpElim _ _ ImpIntr {} _) = Just p
findCut p@(NotElim _ _ NotIntr {} _) = Just p
findCut p@(OrElim _ _ OrIntrL {} _ _) = Just p
findCut p@(OrElim _ _ OrIntrR {} _ _) = Just p
findCut p@(AndElimL _ _ AndIntr {}) = Just p
findCut p@(AndElimR _ _ AndIntr {}) = Just p
findCut p@(IffElimL _ _ IffIntr {}) = Just p
findCut p@(IffElimR _ _ IffIntr {}) = Just p
findCut _ = Nothing

-- | Check if a proof has a cut.
hasCut :: Proof a -> Bool
hasCut = isJust . findCut
