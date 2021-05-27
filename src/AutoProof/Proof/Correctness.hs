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

import AutoProof.AST (AST (root))
import AutoProof.Formula (Formula (And, Iff, Imp, Lit, Not, Or))
import AutoProof.Judgement (Judgement (Judgement))
import AutoProof.Proof.Types
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
import Data.Either (isRight)
import qualified Data.Set as Set

-- | Return an invalid inference node (on the 'Left'), if there is one.
-- Otherwise, return @'Right' ()@.
debug :: Ord a => Proof a -> Either (Proof a) ()
-- Axiom rule: if a belongs to g, then
--
-- ----- (Axiom)
-- g ⊢ a
debug x@(Axiom (Judgement g a)) = if Set.member a g then return () else Left x
-- Falsity elimination:
--
--   p
-- -----
-- g ⊢ ⊥
-- ----- (⊥E)
-- g ⊢ a
debug x@(FalseElim (Judgement g _) p) =
  let Judgement g' f = root p
   in if f == Lit False && g' == g
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
debug x@(NotElim (Judgement g (Lit False)) p q) =
  let Judgement g1 na = root p
      Judgement g2 a = root q
   in case na of
        Not b | b == a && g == Set.union g1 g2 -> do
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
debug x@(NotIntr (Judgement g (Not a)) p) =
  let (Judgement g' f) = root p
   in case f of
        Lit False | g' == Set.insert a g -> debug p
        _ -> Left x
-- Implication elimination: if g is the union of g1 and g2, then
--
--     p           q
-- ----------    ------
-- g1 ⊢ a → b    g2 ⊢ a
-- -------------------- (→E)
--        g ⊢ b
debug x@(ImpElim (Judgement g b) p q) =
  let Judgement g1 c = root p
      Judgement g2 a = root q
   in if c == Imp a b && g == g1 `Set.union` g2
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
debug x@(ImpIntr (Judgement g (Imp a b)) p) =
  let Judgement g' b' = root p
   in if b' == b && g' == Set.insert a g
        then debug p
        else Left x
-- Disjunction elimination: if g is the union of g1, g2, and g3, then
--
--      p            q            r
-- ----------    ---------    ---------
-- g1 ⊢ a ∨ b    g2, a ⊢ c    g3, b ⊢ c
-- ------------------------------------ (∨E)
--                g ⊢ c
debug x@(OrElim (Judgement g c) p q r) =
  let Judgement g1 ab = root p
      Judgement g2 c2 = root q
      Judgement g3 c3 = root r
   in case ab of
        Or a b | c2 == c
                   && c3 == c
                   && a `Set.member` g2
                   && b `Set.member` g3
                   && Set.insert a (Set.insert b g) == Set.union (Set.union g1 g2) g3 ->
          do
            debug p
            debug q
            debug r
        _ -> Left x
-- Disjunction introduction (left):
--
--     p
--   -----
--   g ⊢ a
-- --------- (∨IL)
-- g ⊢ a ∨ b
debug x@(OrIntrL (Judgement g (Or a _)) p) =
  let Judgement g' a' = root p
   in if a' == a && g' == g
        then debug p
        else Left x
-- Disjunction introduction (right):
--
--     p
--   -----
--   g ⊢ b
-- --------- (∨IL)
-- g ⊢ a ∨ b
debug x@(OrIntrR (Judgement g (Or _ b)) p) =
  let Judgement g' b' = root p
   in if b' == b && g' == g
        then debug p
        else Left x
-- Conjunction elimination (left):
--
--     p
-- ---------
-- g ⊢ a ∧ b
-- --------- (∧EL)
--   g ⊢ a
debug x@(AndElimL (Judgement g a) p) =
  let Judgement g' ab = root p
   in case ab of
        And a' _ | a' == a && g' == g -> debug p
        _ -> Left x
-- Conjunction elimination (right):
--
--     p
-- ---------
-- g ⊢ a ∧ b
-- --------- (∧ER)
--   g ⊢ b
debug x@(AndElimR (Judgement g b) p) =
  let Judgement g' ab = root p
   in case ab of
        And _ b' | b' == b && g' == g -> debug p
        _ -> Left x
-- Conjunction introduction: if g is the union of g1 and g2, then
--
--    p          q
-- ------     ------
-- g1 ⊢ a     g2 ⊢ b
-- ----------------- (∧I)
--     g ⊢ a ∧ b
debug x@(AndIntr (Judgement g (And a b)) p q) =
  let Judgement g1 a' = root p
      Judgement g2 b' = root q
   in if a' == a && b' == b && g == Set.union g1 g2
        then do
          debug p
          debug q
        else Left x
-- Equivalence elimination (left):
--
--     p
-- ---------
-- g ⊢ a ↔ b
-- --------- (↔EL)
-- g ⊢ a → b
debug x@(IffElimL (Judgement g (Imp a b)) p) =
  let Judgement g' ab = root p
   in case ab of
        Iff a' b' | a' == a && b' == b && g' == g -> debug p
        _ -> Left x
-- Equivalence elimination (right):
--
--     p
-- ---------
-- g ⊢ a ↔ b
-- --------- (↔EL)
-- g ⊢ b → a
debug x@(IffElimR (Judgement g (Imp b a)) p) =
  let Judgement g' ab = root p
   in case ab of
        Iff a' b' | a' == a && b' == b && g' == g -> debug p
        _ -> Left x
-- Equivalence introduction: if g is the union of g1 and g2, then
--
--      p              q
-- ----------     ----------
-- g1 ⊢ a → b     g2 ⊢ b → a
-- ------------------------- (↔I)
--         g ⊢ a ↔ b
debug x@(IffIntr (Judgement g (Iff a b)) p q) =
  let Judgement g1 ab = root p
      Judgement g2 ba = root q
   in if ab == Imp a b && ba == Imp b a && g == Set.union g1 g2
        then do
          debug p
          debug q
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
    && ( let Judgement c' b = root p
          in a == b && c' `Set.isSubsetOf` c
       )
