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
-- 1. For each propositional formula \(b\), introduce a new propositional
--    variable \(x_b\). Take \(a^* = x_a\).
-- 2. Let \(g\) be the set consisting of formulas of the following form:
--
--     1. (propositional literals)
--        \[
--          x_\top, \qquad
--          x_\bot \rightarrow \bot, \qquad
--          \bot \rightarrow x_\bot, \qquad
--          x_\top \rightarrow \top, \qquad
--          \top \rightarrow x_\top
--        \]
--     2. (variables) for each variable \(y\) in \(a\),
--        \[
--          x_y \rightarrow y, \qquad
--          y \rightarrow x_y
--        \]
--     3. (top introduction and bottom elimination) for each subformula \(b\) of
--        \(a\),
--        \[
--          x_\bot \rightarrow x_b, \qquad
--          x_b \rightarrow x_\top
--        \]
--     4. (negation introduction and elimination) for each subformula \(b\) of
--        \(a\) of the form \(b = \neg c\),
--        \[
--          (x_c \rightarrow x_\bot) \rightarrow x_b, \qquad
--          x_b \rightarrow (x_c \rightarrow x_\bot)
--        \]
--     5. (implication introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \rightarrow d\),
--        \[
--          (x_c \rightarrow x_d) \rightarrow x_b, \qquad
--          x_b \rightarrow (x_c \rightarrow x_d)
--        \]
--     6. (disjunction introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \lor d\), and each additional subformula
--        \(e\) of \(a\),
--        \[
--          x_c \rightarrow x_b, \qquad
--          x_d \rightarrow x_b, \qquad
--          x_b \rightarrow ((x_c \rightarrow x_e) \rightarrow ((x_d \rightarrow x_e) \rightarrow x_e))
--        \]
--     7. (conjunction introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \land d\),
--        \[
--          x_c \rightarrow (x_d \rightarrow x_b), \qquad
--          x_b \rightarrow x_c, \qquad
--          x_b \rightarrow x_d
--        \]
--     8. (equivalence introduction and elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \leftrightarrow d\),
--        \[
--          (x_c \rightarrow x_d) \rightarrow ((x_d \rightarrow x_c) \rightarrow x_b), \qquad
--          x_b \rightarrow (x_c \rightarrow x_d), \qquad
--          x_b \rightarrow (x_d \rightarrow x_c)
--        \]
toImp :: Ord a => Formula a -> Judgement (Either (Formula a) a)
toImp a = Judgement g (new a)
  where
    bs = subformulas a

    -- (new b) represents the variable x_b
    new = var . Left

    -- (old y) represents the variable y
    old = var . Right

    -- Initial context with formulas corresponding to propositional literals
    g0 =
      toDList
        [ new true,
          imp (new false) false,
          imp false (new false),
          imp (new true) true,
          imp true (new true)
        ]

    -- Final context
    g = Set.fromList $ fromDList $ f g0 a

    -- Add common formulas into the growing context
    f c b = h (c . common b) b

    -- Add unique formulas into the growing context
    h c r@(Lit _) = c . unique r
    h c r@(Var _) = c . unique r
    h c r@(Not _ _ p) = f (c . unique r) p
    h c r@(Imp _ _ p q) = f (f (c . unique r) p) q
    h c r@(Or _ _ p q) = f (f (c . unique r) p) q
    h c r@(And _ _ p q) = f (f (c . unique r) p) q
    h c r@(Iff _ _ p q) = f (f (c . unique r) p) q

    -- Formulas that get created for every subformula b of a
    common b = toDList [imp (new false) (new b), imp (new b) (new true)]

    -- Formulas that get created depending on the shape of a subformula b of a
    unique = toDList . unique'
    unique' (Lit _) = []
    unique' b@(Var x) =
      [ imp (old x) (new b),
        imp (new b) (old x)
      ]
    unique' b@(Not _ _ c) =
      [ imp (new b) (imp (new c) (new false)),
        imp (imp (new c) (new false)) (new b)
      ]
    unique' b@(Imp _ _ c d) =
      [ imp (new b) (imp (new c) (new d)),
        imp (imp (new c) (new d)) (new b)
      ]
    unique' b@(Or _ _ c d) =
      let t0 =
            [ imp (new c) (new b),
              imp (new d) (new b)
            ]
          orE e = imp (new b) (imp (imp (new c) (new e)) (imp (imp (new d) (new e)) (new e)))
       in foldr ((:) . orE) t0 bs
    unique' b@(And _ _ c d) =
      [ imp (new c) (imp (new d) (new b)),
        imp (new b) (new c),
        imp (new b) (new d)
      ]
    unique' b@(Iff _ _ c d) =
      [ imp (new b) (imp (new c) (new d)),
        imp (new b) (imp (new d) (new c)),
        imp (imp (new c) (new d)) (imp (imp (new d) (new c)) (new b))
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
