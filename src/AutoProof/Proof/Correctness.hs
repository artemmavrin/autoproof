-- |
-- Module      : AutoProof.Proof.Correctness
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Check proof correctness.
module AutoProof.Proof.Correctness
  ( correct,
    valid,
    debug,
  )
where

import AutoProof.Formula
  ( Formula (Imp, Lit, Not),
    false,
    imp,
  )
import AutoProof.Judgement (Judgement (Judgement))
import AutoProof.Proof.Types
  ( Proof
      ( Ax,
        FalseElim,
        ImpElim,
        ImpIntr,
        NotElim,
        NotIntr,
        TrueIntr
      ),
    judgement,
  )
import Data.Either (isRight)
import qualified Data.Set as Set

-- | Return an invalid inference node (on the 'Left'), if there is one.
-- Otherwise, return @'Right' ()@.
debug :: Ord a => Proof a -> Either (Proof a) ()
-- Axiom rule: if a belongs to g, then
--
-- ----- (Ax)
-- g ⊢ a
debug x@(Ax (Judgement g a)) = if Set.member a g then return () else Left x
-- Falsity elimination:
--
--   p
-- -----
-- g ⊢ ⊥
-- ----- (⊥E)
-- g ⊢ a
debug x@(FalseElim _ (Judgement g _) p) =
  let Judgement g' b' = judgement p
   in if b' == false && g' == g
        then debug p
        else Left x
-- Truth introduction:
--
-- ----- (⊤I)
-- g ⊢ ⊤
debug (TrueIntr (Judgement _ (Lit True))) = return ()
-- Negation elimination: if g is the union of g1 and g2, then
--
--    p          q
-- -------    ------
-- g1 ⊢ ¬a    g2 ⊢ a
-- ----------------- (¬E)
--        g ⊢ ⊥
debug x@(NotElim _ (Judgement g (Lit False)) p q) =
  let Judgement g1 na = judgement p
      Judgement g2 a = judgement q
   in case na of
        Not _ _ b | b == a && g == g1 `Set.union` g2 -> do
          debug p
          debug q
        _ -> Left x
-- Negation introduction:
--
--     p
-- --------
-- g, a ⊢ ⊥
-- -------- (¬I)
--  g ⊢ ¬a
debug x@(NotIntr _ (Judgement g (Not _ _ a)) p) =
  let (Judgement g' b) = judgement p
   in case b of
        Lit False | g' == Set.insert a g -> debug p
        _ -> Left x
-- Implication elimination: if g is the union of g1 and g2, then
--
--     p           q
-- ----------    ------
-- g1 ⊢ a → b    g2 ⊢ a
-- -------------------- (→E)
--        g ⊢ b
debug x@(ImpElim _ (Judgement g b) p q) =
  let Judgement g1 c = judgement p
      Judgement g2 a = judgement q
   in if c == imp a b && g == g1 `Set.union` g2
        then do
          debug p
          debug q
        else Left x
-- Implication introduction:
--
--     p
--  -------
--  g,a ⊢ b
-- --------- (→I)
-- g ⊢ a → b
debug x@(ImpIntr _ (Judgement g (Imp _ _ a b)) p) =
  let Judgement g' b' = judgement p
   in if b' == b && g' == Set.insert a g
        then debug p
        else Left x
-- Pattern match failure:
debug x = Left x

-- | Check whether a proof is valid.
valid :: Ord a => Proof a -> Bool
valid p = isRight (debug p)

-- | Check whether a proof is a correct proof of a given judgement
correct :: Ord a => Judgement a -> Proof a -> Bool
correct (Judgement c a) p =
  valid p
    && ( let Judgement c' b = judgement p
          in a == b && c' `Set.isSubsetOf` c
       )
