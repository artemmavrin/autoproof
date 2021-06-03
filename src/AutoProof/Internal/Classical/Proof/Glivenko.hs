-- |
-- Module      : AutoProof.Internal.Classical.Proof.Glivenko
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Glivenko translation.
module AutoProof.Internal.Classical.Proof.Glivenko
  ( glivenkoTranslate,
    isProvableGlivenko,
  )
where

import AutoProof.Internal.Formula (Formula (Lit, Not))
import qualified AutoProof.Internal.Intuitionistic.Proof as I (isProvable)
import AutoProof.Internal.Judgement (Judgement (Judgement))
import qualified Data.Set as Set (insert)

-- | Translate one judgement into another such that the first is classically
-- provable if and only if the second is intuitionistically provable.
-- Specifically, the Glivenko translation of a judgement \(g \vdash a\) is the
-- judgement \(g, \lnot a \vdash \bot\).
glivenkoTranslate :: Ord a => Judgement a -> Judgement a
glivenkoTranslate (Judgement g a) = Judgement (Set.insert (Not a) g) (Lit False)

-- | Determine whether a judgement is classically valid using Glivenko's
-- translation.
isProvableGlivenko :: Ord a => Judgement a -> Bool
isProvableGlivenko = I.isProvable . glivenkoTranslate
