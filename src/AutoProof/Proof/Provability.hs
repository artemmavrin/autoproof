-- |
-- Module      : AutoProof.Proof.Provability
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Check provability of general propositional formulas
module AutoProof.Proof.Provability (toImp, isTautology) where

import AutoProof.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    false,
    imp,
    subformulas,
    true,
    var,
  )
import AutoProof.Judgement (Judgement (Judgement))
import AutoProof.Proof.Implication (proveImp)
import AutoProof.Utils.DList (fromDList, toDList)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Prelude hiding (and, not, or)

-- | Convert a propositional formula into an implicational judgement which is
-- provable if and only if the original formula is an intuitionistic tautology.
--
-- This construction is due to
--
-- *  Richard Statman (1979)
--    "Intuitionistic propositional logic is polynomial-space complete."
--    Theoretical Computer Science, Volume 9, Issue 1, pp. 67–72.
--    <https://doi.org/10.1016/0304-3975(79)90006-9 DOI>.
--
-- ==== __Details__
--
-- Statman demonstrated how to turn a formula \(a\) in full propositional logic
-- into a judgement \(g \vdash a^*\) in just the implicational fragment of
-- propositional logic such that \(a\) is an intuitionistic tautology if and
-- only if the judgement \(g \vdash a^*\) is provable. This function computes
-- the translation from \(a\) to \(g \vdash a^*\), enabling the checking of
-- whether \(a\) is a tautology by checking the provability of \(g \vdash a^*\)
-- (using, e.g., 'AutoProof.Proof.Implication.proveImp').
--
-- The transformation from \(a\) to \(g \vdash a^*\) involves two steps:
--
-- 1. For each propositional formula \(b\), introduce a var propositional
--    variable \(x_b\). Take \(a^* = x_a\).
-- 2. Let \(g\) be the set consisting of formulas of the following form:
--
--     1. (truth)
--        \[
--          x_\top
--        \]
--     2. (truth introduction and falsity elimination) for each subformula \(b\)
--        of \(a\),
--        \[
--          x_\bot \rightarrow x_b, \qquad
--          x_b \rightarrow x_\top
--        \]
--     3. (negation introduction and elimination) for each subformula \(b\) of
--        \(a\) of the form \(b = \neg c\),
--        \[
--          (x_c \rightarrow x_\bot) \rightarrow x_b, \qquad
--          x_b \rightarrow (x_c \rightarrow x_\bot)
--        \]
--     4. (implication introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \rightarrow d\),
--        \[
--          (x_c \rightarrow x_d) \rightarrow x_b, \qquad
--          x_b \rightarrow (x_c \rightarrow x_d)
--        \]
--     5. (disjunction introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \lor d\), and each additional subformula
--        \(e\) of \(a\),
--        \[
--          x_c \rightarrow x_b, \qquad
--          x_d \rightarrow x_b, \qquad
--          x_b \rightarrow ((x_c \rightarrow x_e) \rightarrow ((x_d \rightarrow x_e) \rightarrow x_e))
--        \]
--     6. (conjunction introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \land d\),
--        \[
--          x_c \rightarrow (x_d \rightarrow x_b), \qquad
--          x_b \rightarrow x_c, \qquad
--          x_b \rightarrow x_d
--        \]
--     7. (equivalence introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \leftrightarrow d\),
--        \[
--          (x_c \rightarrow x_d) \rightarrow ((x_d \rightarrow x_c) \rightarrow x_b), \qquad
--          x_b \rightarrow (x_c \rightarrow x_d), \qquad
--          x_b \rightarrow (x_d \rightarrow x_c)
--        \]
toImp :: Ord a => Formula a -> Judgement (Formula a)
toImp a = Judgement g (var a)
  where
    bs = subformulas a

    -- Final context, created by recursing over subformulas of a
    g = Set.fromList $ fromDList $ f g0 a

    -- Add common formulas into the growing context g'
    f g' b = h (g' . common b) b

    -- Add unique formulas into the growing context g'
    h g' b@(Lit _) = g' . unique b
    h g' b@(Var _) = g' . unique b
    h g' b@(Not _ _ c) = f (g' . unique b) c
    h g' b@(Imp _ _ c d) = f (f (g' . unique b) c) d
    h g' b@(Or _ _ c d) = f (f (g' . unique b) c) d
    h g' b@(And _ _ c d) = f (f (g' . unique b) c) d
    h g' b@(Iff _ _ c d) = f (f (g' . unique b) c) d

    -- Initial context
    g0 = toDList [var true]

    -- Formulas that get created for every subformula b of a
    common b = toDList [imp (var false) (var b), imp (var b) (var true)]

    -- Formulas that get created depending on the shape of a subformula b of a
    unique = toDList . unique'
    unique' (Lit _) = []
    unique' (Var _) = []
    unique' b@(Not _ _ c) =
      [ imp (var b) (imp (var c) (var false)),
        imp (imp (var c) (var false)) (var b)
      ]
    unique' b@(Imp _ _ c d) =
      [ imp (var b) (imp (var c) (var d)),
        imp (imp (var c) (var d)) (var b)
      ]
    unique' b@(Or _ _ c d) =
      let t0 =
            [ imp (var c) (var b),
              imp (var d) (var b)
            ]
          orE e = imp (var b) (imp (imp (var c) (var e)) (imp (imp (var d) (var e)) (var e)))
       in foldr ((:) . orE) t0 bs
    unique' b@(And _ _ c d) =
      [ imp (var c) (imp (var d) (var b)),
        imp (var b) (var c),
        imp (var b) (var d)
      ]
    unique' b@(Iff _ _ c d) =
      [ imp (var b) (imp (var c) (var d)),
        imp (var b) (imp (var d) (var c)),
        imp (imp (var c) (var d)) (imp (imp (var d) (var c)) (var b))
      ]

-- | Determine whether a formula is an intuitionistic tautology.
--
-- The algorithm is due to
--
-- *  Richard Statman (1979)
--    "Intuitionistic propositional logic is polynomial-space complete."
--    Theoretical Computer Science, Volume 9, Issue 1, pp. 67–72.
--    <https://doi.org/10.1016/0304-3975(79)90006-9 DOI>.
--
-- ==== __Examples__
--
-- >>> isTautology $ imp (and (var 'a') (var 'b')) (var 'a')
-- True
--
-- >>> isTautology $ or (var 'a') (not (var 'a'))
-- False
--
-- >>> isTautology $ not (not (or (var 'a') (not (var 'a'))))
-- True
isTautology :: Ord a => Formula a -> Bool
isTautology = isJust . proveImp . toImp
