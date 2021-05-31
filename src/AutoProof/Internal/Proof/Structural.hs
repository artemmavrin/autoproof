-- |
-- Module      : AutoProof.Internal.Proof.Structural
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Structural rules to transform proofs.
module AutoProof.Internal.Proof.Structural
  ( weakenProof,
    strengthenProof,
  )
where

import AutoProof.Internal.AST (AST (height, root))
import AutoProof.Internal.Formula (Formula (Imp, Not, Or))
import AutoProof.Internal.Judgement
  ( Judgement
      ( Judgement,
        antecedents,
        succedent
      ),
    weakenJudgement,
  )
import AutoProof.Internal.Proof.Types
  ( Proof
      ( AndElimL,
        AndElimR,
        AndIntr,
        Axiom,
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
  )
import qualified Data.Set as Set

-- | The /weakening/ structural rule. @('weakenProof' p a)@ modifies the proof
-- @p@ to include @a@ as an additional hypothesis.
weakenProof :: Ord a => Proof a -> Formula a -> Proof a
weakenProof (Axiom j) a = weakenNullary Axiom a j
weakenProof x@(FalseElim j p) a = weakenUnary FalseElim x a j p
weakenProof (TrueIntr j) a = weakenNullary TrueIntr a j
weakenProof x@(NotElim j p q) a = weakenBinary NotElim x a j p q
weakenProof x@(NotIntr j p) a = weakenUnary NotIntr x a j p
weakenProof x@(ImpElim j p q) a = weakenBinary ImpElim x a j p q
weakenProof x@(ImpIntr j p) a = weakenUnary ImpIntr x a j p
weakenProof x@(OrElim j p q r) a = weakenTernary OrElim x a j p q r
weakenProof x@(OrIntrL j p) a = weakenUnary OrIntrL x a j p
weakenProof x@(OrIntrR j p) a = weakenUnary OrIntrR x a j p
weakenProof x@(AndElimL j p) a = weakenUnary AndElimL x a j p
weakenProof x@(AndElimR j p) a = weakenUnary AndElimR x a j p
weakenProof x@(AndIntr j p q) a = weakenBinary AndIntr x a j p q
weakenProof x@(IffElimL j p) a = weakenUnary IffElimL x a j p
weakenProof x@(IffElimR j p) a = weakenUnary IffElimR x a j p
weakenProof x@(IffIntr j p q) a = weakenBinary IffIntr x a j p q

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
strengthenProof (Axiom (Judgement _ a)) = Axiom (Judgement (Set.singleton a) a)
strengthenProof (FalseElim j p) = strengthenUnary FalseElim j p
strengthenProof (TrueIntr (Judgement _ a)) = TrueIntr (Judgement Set.empty a)
strengthenProof (NotElim j p q) = strengthenBinary NotElim j p q
strengthenProof (NotIntr (Judgement _ a@(Not b)) p) = strengthenUnaryImp NotIntr a b p
strengthenProof (ImpElim j p q) = strengthenBinary ImpElim j p q
strengthenProof (ImpIntr (Judgement _ a@(Imp b _)) p) = strengthenUnaryImp ImpIntr a b p
strengthenProof (OrElim j p q r) = strengthenOrElim j p q r
strengthenProof (OrIntrL j p) = strengthenUnary OrIntrL j p
strengthenProof (OrIntrR j p) = strengthenUnary OrIntrR j p
strengthenProof (AndElimL j p) = strengthenUnary AndElimL j p
strengthenProof (AndElimR j p) = strengthenUnary AndElimR j p
strengthenProof (AndIntr j p q) = strengthenBinary AndIntr j p q
strengthenProof (IffElimL j p) = strengthenUnary IffElimL j p
strengthenProof (IffElimR j p) = strengthenUnary IffElimR j p
strengthenProof (IffIntr j p q) = strengthenBinary IffIntr j p q
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
  Or a b ->
    let p' = strengthenProof p
        q' = strengthenProof q
        r' = strengthenProof r
        q'' = weakenProof q' a -- a might not be needed in q'
        r'' = weakenProof r' b -- b might not be needed in r'
        g1 = antecedents (root p')
        g2 = Set.delete a (antecedents (root q''))
        g3 = Set.delete b (antecedents (root r''))
        g = Set.union (Set.union g1 g2) g3
     in OrElim (Judgement g c) p' q'' r''
  _ -> OrElim j p q r -- Wrong form of inference rule!
