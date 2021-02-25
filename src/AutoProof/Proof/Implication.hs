-- |
-- Module      : AutoProof.Proof.Implication
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic natural deduction proofs in the implicational fragment of
-- propositional logic.
module AutoProof.Proof.Implication
  ( proveImp,
  )
where

import AutoProof.Formula (Formula (Imp, Var), imp)
import AutoProof.Judgement (Judgement (Judgement))
import AutoProof.Proof.Types (Proof, axiom, impElim, impIntr)
import qualified Data.Set as Set

-- | @('proveImp' (c 'AutoProof.Judgement.|-' a))@ finds an intuitionistic proof
-- of a judgement \(c \vdash a\) in the implicational fragment of propositional
-- logic, if such a proof exists.
--
-- The algorithm is adapted from section 2.4 of
--
-- *  Samuel Mimram (2020)
--    /PROGRAM = PROOF/.
proveImp :: Ord a => Judgement a -> Maybe (Proof a)
proveImp (Judgement g f) = prove Set.empty g f
  where
    -- We search for a proof while maintaining a set @s@ of previously seen
    -- judgements to avoid cycles
    --
    -- Easy case: if there is a proof of c ⊢ a → b, then there must be a proof
    -- of c ⊢ a → b that ends with an inference of the form
    --
    --  c,a ⊢ b
    -- --------- (→I)
    -- c ⊢ a → b
    --
    -- so it suffices to look for a proof of c,a ⊢ b
    prove s c i@(Imp _ _ a b) =
      let j = Judgement c i
       in if Set.member i c
            then Just $ axiom j
            else impIntr j <$> prove (Set.insert (i, c) s) (Set.insert a c) b
    -- Trickier case: if there is a proof of c ⊢ x, where x is a variable, then
    -- either
    --
    -- (1) x belongs to c, so c ⊢ x is proved via
    --
    -- ----- (Ax)
    -- c ⊢ x
    --
    -- or
    --
    -- (2) there is a sequence a1,...,an of propositional formulas such that
    -- a1 → (a2 → (... an → x)...) belongs to c, and c ⊢ ai for all i=1,...,n.
    -- In this case, one proof of c ⊢ x is
    --
    --                                         p1
    -- ------------------------------- (Ax)  ------
    -- c ⊢ a1 → (a2 → (... an → x)...)       c ⊢ a1         p2
    -- -------------------------------------------- (→E)  ------
    --        c ⊢ a2 → (... an → x)                       c ⊢ a2
    --                                 ...                           pn
    --                                                             ------
    --                             c ⊢ an → x                      c ⊢ an
    -- ------------------------------------------------------------------ (→E)
    --                               c ⊢ x
    --
    -- Actually, case (1) can be thought as the n=0 case of case (2).
    --
    -- The implementation:
    prove s c v@(Var x) =
      if Set.member (v, c) s
        then Nothing -- Already visited current judgement; avoid cycles
        else foldl findImp Nothing c
      where
        -- Save the current judgement so we don't revisit it recursively
        s' = Set.insert (v, c) s

        -- findImp is folded over the context looking for implications ending in
        -- the variable x. When encountering a formula of the form
        -- a1 → (a2 → (... an → x)...), ensure each ai is provable, and, if so,
        -- construct a proof.
        findImp p@(Just _) _ = p -- Already found a proof!
        findImp Nothing a = do
          as <- splitImp a
          construct as v

        -- Given a formula of the form a1 → (a2 → (... an → x)...), extract the
        -- list [an, ..., a2, a1]. For formulas of all other forms, return
        -- Nothing.
        splitImp = go []
          where
            go l (Var y) = if x == y then Just l else Nothing
            go l (Imp _ _ a b) = go (a : l) b
            go _ _ = Nothing

        -- Given a list of formulas [an, ..., a2, a1] from an implication of the
        -- form a1 → (a2 → (... an → x)...), try to prove the ai's, and use the
        -- resulting proofs to construct a proof of x
        construct [] b = Just $ axiom (Judgement c b)
        construct (a : as) b =
          impElim (Judgement c b) <$> construct as (imp a b) <*> prove s' c a

    -- Non-implicational case
    prove _ _ _ = Nothing
