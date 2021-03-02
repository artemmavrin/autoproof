-- |
-- Module      : AutoProof.Proof.Transform
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- New proofs from old.
module AutoProof.Proof.Transform
  ( weakenProof,
    strengthenProof,
  )
where

import AutoProof.Formula (Formula (Imp))
import AutoProof.Judgement (Judgement (Judgement, antecedents), weakenJudgement)
import AutoProof.Proof.Types
  ( Proof
      ( Ax,
        FalseElim,
        ImpElim,
        ImpIntr,
        NotElim,
        TrueIntr
      ),
    axiom,
    falseElim,
    height,
    impElim,
    impIntr,
    judgement,
    notElim,
    trueIntr,
  )
import qualified Data.Set as Set

-- | The /weakening/ structural rule. @('weakenProof' p a)@ modifies the proof
-- @p@ to include @a@ as an additional hypothesis.
weakenProof :: Ord a => Proof a -> Formula a -> Proof a
weakenProof (Ax j) a = axiom (weakenJudgement j a)
weakenProof (FalseElim _ j p) a = weakenUnary falseElim a j p
weakenProof (TrueIntr j) a = trueIntr (weakenJudgement j a)
weakenProof x@(NotElim _ j p q) a = weakenBinary notElim x a j p q
weakenProof x@(ImpElim _ j p q) a = weakenBinary impElim x a j p q
weakenProof (ImpIntr _ j p) a = weakenUnary impIntr a j p

-- Helper functions for weakenProof
weakenUnary ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a) ->
  Formula a ->
  Judgement a ->
  Proof a ->
  Proof a
weakenUnary c a j p = c (weakenJudgement j a) (weakenProof p a)

weakenBinary ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a -> Proof a) ->
  Proof a ->
  Formula a ->
  Judgement a ->
  Proof a ->
  Proof a ->
  Proof a
weakenBinary c x a j p q
  | a `Set.member` antecedents j = x
  | height p < height q = c (weakenJudgement j a) (weakenProof p a) q
  | otherwise = c (weakenJudgement j a) p (weakenProof q a)

-- | Strengthen a proof by preserving its structure but removing redundant
-- hypotheses where possible.
strengthenProof :: Ord a => Proof a -> Proof a
strengthenProof (Ax (Judgement _ a)) = axiom (Judgement (Set.singleton a) a)
strengthenProof (FalseElim _ (Judgement _ a) p) =
  let p' = strengthenProof p
      c = antecedents (judgement p')
   in falseElim (Judgement c a) p'
strengthenProof (TrueIntr (Judgement _ a)) = trueIntr (Judgement Set.empty a)
strengthenProof (NotElim _ (Judgement _ a) p q) =
  let p' = strengthenProof p
      q' = strengthenProof q
      cp = antecedents (judgement p')
      cq = antecedents (judgement q')
      c = Set.union cp cq
   in notElim (Judgement c a) p' q'
strengthenProof (ImpElim _ (Judgement _ a) p q) =
  let p' = strengthenProof p
      q' = strengthenProof q
      cp = antecedents (judgement p')
      cq = antecedents (judgement q')
      c = Set.union cp cq
   in impElim (Judgement c a) p' q'
strengthenProof (ImpIntr _ (Judgement _ i@(Imp _ _ a _)) p) =
  let p' = strengthenProof p
      p'' = weakenProof p' a -- a might not be needed in p'
      c = antecedents (judgement p'')
      c' = Set.delete a c
   in impIntr (Judgement c' i) p''
-- If we're here, then the proof is invalid!
strengthenProof _ = undefined
