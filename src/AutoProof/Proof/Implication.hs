-- |
-- Module      : AutoProof.Proof.Implication
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic proofs in the implicational fragment of propositional logic.
module AutoProof.Proof.Implication
  ( proveImp,
  )
where

import AutoProof.Formula (Formula (Imp, Var), imp)
import AutoProof.Judgement (Judgement (Judgement))
import AutoProof.Proof.Types (Proof, axiom, impElim, impIntr)
import Control.Applicative ((<|>))
import qualified Data.Set as Set

-- | @('proveImp' (g 'AutoProof.Judgement.|-' a))@ finds an intuitionistic proof
-- of a judgement \(g \vdash a\) in the implicational fragment of propositional
-- logic, if such a proof exists.
--
-- The algorithm is adapted from section 2.4 of
--
-- *  Samuel Mimram (2020)
--    /PROGRAM = PROOF/.
--
-- ==== __Examples__
--
-- >>> proveImp $ [var 'a', imp (var 'a') (var 'b')] |- var 'b'
-- Just (impElim ([var 'a',imp (var 'a') (var 'b')] |- var 'b') (axiom ([var 'a',imp (var 'a') (var 'b')] |- imp (var 'a') (var 'b'))) (axiom ([var 'a',imp (var 'a') (var 'b')] |- var 'a')))
--
-- >>> proveImp $ [imp (var 'a') (var 'b'), imp (var 'b') (var 'a')] |- var 'a'
-- Nothing
proveImp :: Ord a => Judgement a -> Maybe (Proof a)
proveImp = prove Set.empty
  where
    -- We search for a proof while maintaining a set s of previously seen
    -- judgements of the form g ⊢ x, for x a variable, to avoid cycles

    -- Easy case: if there is a proof of g ⊢ a → b, then there must be a proof
    -- proof of g ⊢ a → b that ends with an inference of the form
    --
    --  g,a ⊢ b
    -- --------- (→I)
    -- g ⊢ a → b
    --
    -- so it suffices to look for a proof of g,a ⊢ b
    prove s j@(Judgement g i@(Imp _ a b)) =
      if Set.member i g
        then Just $ axiom j
        else impIntr j <$> prove s (Judgement (Set.insert a g) b)
    -- Trickier case: if there is a proof of g ⊢ x, where x is a variable, then
    -- either
    --
    -- (1) x belongs to g, so g ⊢ x is proved via
    --
    -- ----- (Ax)
    -- g ⊢ x
    --
    -- or
    --
    -- (2) there is a sequence a1,...,an of propositional formulas such that
    -- a1 → (a2 → (... an → x)...) belongs to g, and g ⊢ ai for all i=1,...,n.
    -- In this case, one proof of g ⊢ x is
    --
    --                                         p1
    -- ------------------------------- (Ax)  ------
    -- g ⊢ a1 → (a2 → (... an → x)...)       g ⊢ a1         p2
    -- -------------------------------------------- (→E)  ------
    --        g ⊢ a2 → (... an → x)                       g ⊢ a2
    --                                 ...                           pn
    -- ----------------------------------------------------- (→E)  ------
    --                             g ⊢ an → x                      g ⊢ an
    -- ------------------------------------------------------------------ (→E)
    --                               g ⊢ x
    --
    -- Actually, case (1) is the n=0 case of case (2).
    --
    -- The implementation:
    prove s j@(Judgement g v@(Var _ x)) =
      if Set.member j s
        then Nothing -- Already visited current judgement; skip to avoid cycles
        else foldr ((<|>) . findImp) Nothing g -- Scan context looking for proof
      where
        -- Save the current judgement so we don't revisit it recursively
        s' = Set.insert j s

        -- findImp searches each formula in the context for implications ending
        -- in the variable x. When encountering a formula of the form
        -- a1 → (a2 → (... an → x)...), ensure each ai is provable, and, if so,
        -- construct a proof.
        findImp a = do
          as <- splitImp a
          construct as v

        -- Given a formula of the form a1 → (a2 → (... an → x)...), extract the
        -- list [an, ..., a2, a1] (note the reverse order). For formulas of all
        -- other forms, return Nothing.
        splitImp = go []
          where
            go l (Var _ y) = if x == y then Just l else Nothing
            go l (Imp _ a b) = go (a : l) b
            go _ _ = Nothing -- non-implicational case

        -- Given a list of formulas [an, ..., a2, a1] from an implication of the
        -- form a1 → (a2 → (... an → x)...), try to prove the ai's, and use the
        -- resulting proofs to construct a proof of x using nested implication
        -- eliminations
        construct [] b = Just $ axiom (Judgement g b)
        construct (a : as) b = do
          q <- prove s' (Judgement g a)
          p <- construct as (imp a b)
          return $ impElim (Judgement g b) p q

    -- Non-implicational case
    prove _ _ = Nothing
