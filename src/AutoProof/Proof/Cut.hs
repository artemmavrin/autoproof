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

import AutoProof.Proof.Types (Proof (ImpElim, ImpIntr))
import Data.Maybe (isJust)

-- | Find the cut nearest the root of a proof, if any.
findCut :: Proof a -> Maybe (Proof a)
findCut p@(ImpElim _ _ ImpIntr {} _) = Just p
findCut _ = Nothing

-- | Check if a proof has a cut.
hasCut :: Proof a -> Bool
hasCut = isJust . findCut
