-- |
-- Module      : Data.Prop.Proof.Debug
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Proof debugging functions.
module Data.Prop.Proof.Debug
  ( -- * Debugging
    debug,
    valid,
    conclusion,
    context,
  )
where

import Data.Either (isRight)
import Data.Prop.Proof.Types
  ( Proof
      ( AndElimL,
        AndElimR,
        AndIntr,
        Ax,
        BotElim,
        ImpElim,
        ImpIntr,
        NotElim,
        NotIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        TopIntr
      ),
  )
import Data.Prop.Types (Context, Formula (And, Imp, Lit, Not, Or))
import qualified Data.Set as Set

-- | Return an invalid inference node (on the 'Left'), if there is one.
-- Otherwise, return @'Right' ()@.
debug :: Ord a => Proof a -> Either (Proof a) ()
-- Axiom rule: If a belongs to g, then
--
-- ----- (Ax)
-- g ⊢ a
debug x@(Ax g a) = if Set.member a g then return () else Left x
-- Truth introduction:
--
-- ----- (⊤I)
-- g ⊢ ⊤
debug (TopIntr _ (Lit True)) = return ()
-- Falsity elimination:
--
-- g ⊢ ⊥
-- ----- (⊥E)
-- g ⊢ a
debug x@(BotElim g _ p) = case conclusion p of
  Lit False | context p == g -> debug p
  _ -> Left x
-- Negation elimination:
--
-- g ⊢ ¬a    g ⊢ a
-- --------------- (¬E)
--      g ⊢ ⊥
debug x@(NotElim g (Lit False) p q) = case conclusion p of
  Not a | conclusion q == a && context p == g && context q == g ->
    do
      debug p
      debug q
  _ -> Left x
-- Negation introduction:
--
-- g, a ⊢ ⊥
-- -------- (¬I)
--  g ⊢ ¬a
debug x@(NotIntr g (Not a) p) = case conclusion p of
  Lit False | context p == Set.insert a g -> debug p
  _ -> Left x
-- Implication elimination:
--
-- g ⊢ a → b    g ⊢ a
-- ------------------ (→E)
--       g ⊢ b
debug x@(ImpElim g b p q) =
  if conclusion p == Imp (conclusion q) b
    && g == context p
    && g == context q
    then do
      debug p
      debug q
    else Left x
-- Implication introduction:
--
--  g,a ⊢ b
-- --------- (→I)
-- g ⊢ a → b
debug x@(ImpIntr g (Imp a b) p) =
  if conclusion p == b && context p == Set.insert a g
    then debug p
    else Left x
-- Disjunction elimination:
--
-- g ⊢ a ∨ b    g, a ⊢ c    g, b ⊢ c
-- --------------------------------- (∨E)
--               g ⊢ c
debug x@(OrElim g c p q r) =
  case conclusion p of
    Or a b | conclusion q == c
               && conclusion r == c
               && context p == g
               && context q == Set.insert a g
               && context r == Set.insert b g ->
      do
        debug p
        debug q
        debug r
    _ -> Left x
-- Disjunction introduction (left):
--
--   g ⊢ a
-- --------- (∨IL)
-- g ⊢ a ∨ b
debug x@(OrIntrL g (Or a _) p) =
  if conclusion p == a && context p == g
    then debug p
    else Left x
-- Disjunction introduction (right):
--
--   g ⊢ b
-- --------- (∨IR)
-- g ⊢ a ∨ b
debug x@(OrIntrR g (Or _ b) p) =
  if conclusion p == b && context p == g
    then debug p
    else Left x
-- Conjunction elimination (left):
--
-- g ⊢ a ∧ b
-- --------- (∧EL)
--   g ⊢ a
debug x@(AndElimL g a p) =
  case conclusion p of
    And a' _ | a == a' && context p == g -> debug p
    _ -> Left x
-- Conjunction elimination (right):
--
-- g ⊢ a ∧ b
-- --------- (∧EL)
--   g ⊢ b
debug x@(AndElimR g b p) =
  case conclusion p of
    And _ b' | b == b' && context p == g -> debug p
    _ -> Left x
-- Conjunction introduction:
--
-- g ⊢ a     g ⊢ b
-- --------------- (∧I)
--    g ⊢ a ∧ b
debug x@(AndIntr g (And a b) p q) =
  if conclusion p == a
    && conclusion q == b
    && context p == g
    && context q == g
    then do
      debug p
      debug q
    else Left x
-- Pattern match failure:
debug x = Left x

-- | Check whether a proof is valid.
valid :: Ord a => Proof a -> Bool
valid p = isRight (debug p)

-- | Extract the conclusion (without the context) of a proof
conclusion :: Proof a -> Formula a
conclusion (Ax _ a) = a
conclusion (TopIntr _ a) = a
conclusion (BotElim _ a _) = a
conclusion (NotElim _ a _ _) = a
conclusion (NotIntr _ a _) = a
conclusion (ImpElim _ a _ _) = a
conclusion (ImpIntr _ a _) = a
conclusion (OrElim _ a _ _ _) = a
conclusion (OrIntrL _ a _) = a
conclusion (OrIntrR _ a _) = a
conclusion (AndElimL _ a _) = a
conclusion (AndElimR _ a _) = a
conclusion (AndIntr _ a _ _) = a

-- | Extract the final context of a proof
context :: Proof a -> Context a
context (Ax c _) = c
context (TopIntr c _) = c
context (BotElim c _ _) = c
context (NotElim c _ _ _) = c
context (NotIntr c _ _) = c
context (ImpElim c _ _ _) = c
context (ImpIntr c _ _) = c
context (OrElim c _ _ _ _) = c
context (OrIntrL c _ _) = c
context (OrIntrR c _ _) = c
context (AndElimL c _ _) = c
context (AndElimR c _ _) = c
context (AndIntr c _ _ _) = c
