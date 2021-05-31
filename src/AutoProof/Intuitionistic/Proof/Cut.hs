-- |
-- Module      : AutoProof.Intuitionistic.Proof.Cut
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions related to cuts in proofs.
module AutoProof.Intuitionistic.Proof.Cut (findCut, hasCut) where

import AutoProof.Internal.AST (children)
import AutoProof.Internal.Proof.Types
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
import Control.Applicative ((<|>))
import Data.Maybe (isJust)

-- | Find the cut nearest the root of a proof, if any. This functions assumes
-- the proof is valid.
findCut :: Proof a -> Maybe (Proof a)
findCut p@(ImpElim _ ImpIntr {} _) = Just p
findCut p@(NotElim _ NotIntr {} _) = Just p
findCut p@(OrElim _ OrIntrL {} _ _) = Just p
findCut p@(OrElim _ OrIntrR {} _ _) = Just p
findCut p@(AndElimL _ AndIntr {}) = Just p
findCut p@(AndElimR _ AndIntr {}) = Just p
findCut p@(IffElimL _ IffIntr {}) = Just p
findCut p@(IffElimR _ IffIntr {}) = Just p
findCut p = foldr ((<|>) . findCut) Nothing (children p)

-- | Check if a proof has a cut.
hasCut :: Proof a -> Bool
hasCut = isJust . findCut
