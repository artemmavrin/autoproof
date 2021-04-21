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

import AutoProof.AST (AST (height, root))
import AutoProof.Formula (Formula (Imp, Not, Or))
import AutoProof.Judgement
  ( Judgement
      ( Judgement,
        antecedents,
        succedent
      ),
    weakenJudgement,
  )
import AutoProof.Proof.Types
  ( Proof
      ( AndElimL,
        AndElimR,
        AndIntr,
        Ax,
        FalseElim,
        IffElimL,
        IffElimR,
        IffIntr,
        ImpElim,
        ImpIntr,
        NotElim,
        NotIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        TrueIntr
      ),
    andElimL,
    andElimR,
    andIntr,
    axiom,
    falseElim,
    iffElimL,
    iffElimR,
    iffIntr,
    impElim,
    impIntr,
    notElim,
    notIntr,
    orElim,
    orIntrL,
    orIntrR,
    trueIntr,
  )
import qualified Data.Set as Set

-- | The /weakening/ structural rule. @('weakenProof' p a)@ modifies the proof
-- @p@ to include @a@ as an additional hypothesis.
weakenProof :: Ord a => Proof a -> Formula a -> Proof a
weakenProof (Ax _ j) a = weakenNullary axiom a j
weakenProof x@(FalseElim _ j p) a = weakenUnary falseElim x a j p
weakenProof (TrueIntr _ j) a = weakenNullary trueIntr a j
weakenProof x@(NotElim _ j p q) a = weakenBinary notElim x a j p q
weakenProof x@(NotIntr _ j p) a = weakenUnary notIntr x a j p
weakenProof x@(ImpElim _ j p q) a = weakenBinary impElim x a j p q
weakenProof x@(ImpIntr _ j p) a = weakenUnary impIntr x a j p
weakenProof x@(OrElim _ j p q r) a = weakenTernary orElim x a j p q r
weakenProof x@(OrIntrL _ j p) a = weakenUnary orIntrL x a j p
weakenProof x@(OrIntrR _ j p) a = weakenUnary orIntrR x a j p
weakenProof x@(AndElimL _ j p) a = weakenUnary andElimL x a j p
weakenProof x@(AndElimR _ j p) a = weakenUnary andElimR x a j p
weakenProof x@(AndIntr _ j p q) a = weakenBinary andIntr x a j p q
weakenProof x@(IffElimL _ j p) a = weakenUnary iffElimL x a j p
weakenProof x@(IffElimR _ j p) a = weakenUnary iffElimR x a j p
weakenProof x@(IffIntr _ j p q) a = weakenBinary iffIntr x a j p q

-- Helper functions for weakenProof

weakenNullary ::
  Ord a =>
  (Judgement a -> Proof a) ->
  Formula a ->
  Judgement a ->
  Proof a
weakenNullary c a j = c (weakenJudgement j a)

weakenUnary ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a) ->
  Proof a ->
  Formula a ->
  Judgement a ->
  Proof a ->
  Proof a
weakenUnary c x a j@(Judgement g _) p
  | a `Set.member` g = x
  | otherwise = c (weakenJudgement j a) (weakenProof p a)

weakenBinary ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a -> Proof a) ->
  Proof a ->
  Formula a ->
  Judgement a ->
  Proof a ->
  Proof a ->
  Proof a
weakenBinary c x a j@(Judgement g _) p q
  | a `Set.member` g = x
  | a `Set.member` antecedents (root p) = c (weakenJudgement j a) p q
  | a `Set.member` antecedents (root q) = c (weakenJudgement j a) p q
  | height p <= height q = c (weakenJudgement j a) (weakenProof p a) q
  | otherwise = c (weakenJudgement j a) p (weakenProof q a)

weakenTernary ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a -> Proof a -> Proof a) ->
  Proof a ->
  Formula a ->
  Judgement a ->
  Proof a ->
  Proof a ->
  Proof a ->
  Proof a
weakenTernary c x a j@(Judgement g _) p q r
  | a `Set.member` g = x
  | a `Set.member` antecedents (root p) = c (weakenJudgement j a) p q r
  | a `Set.member` antecedents (root q) = c (weakenJudgement j a) p q r
  | a `Set.member` antecedents (root r) = c (weakenJudgement j a) p q r
  | height p <= min (height q) (height r) = c (weakenJudgement j a) (weakenProof p a) q r
  | height q <= min (height p) (height r) = c (weakenJudgement j a) p (weakenProof q a) r
  | otherwise = c (weakenJudgement j a) p q (weakenProof r a)

-- | Strengthen a proof by preserving its structure but removing redundant
-- hypotheses where possible.
strengthenProof :: Ord a => Proof a -> Proof a
strengthenProof (Ax _ (Judgement _ a)) = axiom (Judgement (Set.singleton a) a)
strengthenProof (FalseElim _ j p) = strengthenUnary falseElim j p
strengthenProof (TrueIntr _ (Judgement _ a)) = trueIntr (Judgement Set.empty a)
strengthenProof (NotElim _ j p q) = strengthenBinary notElim j p q
strengthenProof (NotIntr _ (Judgement _ a@(Not _ b)) p) = strengthenUnaryImp notIntr a b p
strengthenProof (ImpElim _ j p q) = strengthenBinary impElim j p q
strengthenProof (ImpIntr _ (Judgement _ a@(Imp _ b _)) p) = strengthenUnaryImp impIntr a b p
strengthenProof (OrElim _ j p q r) = strengthenOrElim j p q r
strengthenProof (OrIntrL _ j p) = strengthenUnary orIntrL j p
strengthenProof (OrIntrR _ j p) = strengthenUnary orIntrR j p
strengthenProof (AndElimL _ j p) = strengthenUnary andElimL j p
strengthenProof (AndElimR _ j p) = strengthenUnary andElimR j p
strengthenProof (AndIntr _ j p q) = strengthenBinary andIntr j p q
strengthenProof (IffElimL _ j p) = strengthenUnary iffElimL j p
strengthenProof (IffElimR _ j p) = strengthenUnary iffElimR j p
strengthenProof (IffIntr _ j p q) = strengthenBinary iffIntr j p q
-- If we're here, then there was a pattern match failure, and the proof is
-- invalid! Note that there are some invalid proofs that will nevertheless slip
-- through this function; its goal is not to check validity. However, a valid
-- input proof should yield a valid output proof.
strengthenProof p = p

-- Helper functions for strengthenProof

strengthenUnary ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a) ->
  Judgement a ->
  Proof a ->
  Proof a
strengthenUnary c (Judgement _ a) p =
  let p' = strengthenProof p
      g = antecedents (root p')
   in c (Judgement g a) p'

strengthenBinary ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a -> Proof a) ->
  Judgement a ->
  Proof a ->
  Proof a ->
  Proof a
strengthenBinary c (Judgement _ a) p q =
  let p' = strengthenProof p
      q' = strengthenProof q
      gp = antecedents (root p')
      gq = antecedents (root q')
      g = Set.union gp gq
   in c (Judgement g a) p' q'

-- Implication and negation introduction rules involve removing a hypothesis
-- and must be treated as a special case
strengthenUnaryImp ::
  Ord a =>
  (Judgement a -> Proof a -> Proof a) ->
  Formula a ->
  Formula a ->
  Proof a ->
  Proof a
strengthenUnaryImp c a b p =
  let p' = strengthenProof p
      p'' = weakenProof p' b -- b might not be needed in p'
      g = antecedents (root p'')
      g' = Set.delete b g
   in c (Judgement g' a) p''

strengthenOrElim ::
  Ord a =>
  Judgement a ->
  Proof a ->
  Proof a ->
  Proof a ->
  Proof a
strengthenOrElim j@(Judgement _ c) p q r = case succedent (root p) of
  Or _ a b ->
    let p' = strengthenProof p
        q' = strengthenProof q
        r' = strengthenProof r
        q'' = weakenProof q' a -- a might not be needed in q'
        r'' = weakenProof r' b -- b might not be needed in r'
        g1 = antecedents (root p')
        g2 = Set.delete a (antecedents (root q''))
        g3 = Set.delete b (antecedents (root r''))
        g = Set.union (Set.union g1 g2) g3
     in orElim (Judgement g c) p' q'' r''
  _ -> orElim j p q r -- Wrong form of inference rule!
