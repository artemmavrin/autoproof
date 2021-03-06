-- |
-- Module      : AutoProof.Proof.Provability
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Check provability of general propositional formulas
module AutoProof.Proof.Provability (toImp, isTautology, proveTautology) where

import AutoProof.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    and,
    false,
    iff,
    imp,
    not,
    or,
    subformulas,
    true,
    var,
  )
import AutoProof.Judgement (Judgement (..), (|-))
import AutoProof.Proof.Implication (proveImp)
import AutoProof.Proof.Transform (strengthenProof)
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
    judgement,
    notElim,
    notIntr,
    orElim,
    orIntrL,
    orIntrR,
    premises,
    trueIntr,
  )
import AutoProof.Utils.DList (fromDList, toDList)
import Control.Applicative (Alternative ((<|>)))
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust)
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

    -- Initial context (vacuous truth introduction)
    g0 = toDList [var true]

    -- Formulas that get created for every subformula b of a
    common b =
      toDList
        [ imp (var false) (var b), -- falsity elimination
          imp (var b) (var true) -- truth introduction
        ]

    -- Formulas that get created depending on the shape of a subformula b of a
    unique = toDList . unique'
    unique' (Lit _) = []
    unique' (Var _) = []
    unique' b@(Not _ _ c) =
      [ imp (var b) (imp (var c) (var false)), -- negation elimination
        imp (imp (var c) (var false)) (var b) -- negation introduction
      ]
    unique' b@(Imp _ _ c d) =
      [ imp (var b) (imp (var c) (var d)), -- implication elimination
        imp (imp (var c) (var d)) (var b) -- implication introduction
      ]
    unique' b@(Or _ _ c d) =
      let t0 =
            [ imp (var c) (var b), -- left disjunction introduction
              imp (var d) (var b) -- right disjunction introduction
            ]
          -- disjunction elimination
          orE e = imp (var b) (imp (imp (var c) (var e)) (imp (imp (var d) (var e)) (var e)))
       in foldr ((:) . orE) t0 bs
    unique' b@(And _ _ c d) =
      [ imp (var c) (imp (var d) (var b)), -- conjunction introduction
        imp (var b) (var c), -- left conjunction elimination
        imp (var b) (var d) -- right conjunction elimination
      ]
    unique' b@(Iff _ _ c d) =
      [ imp (imp (var c) (var d)) (imp (imp (var d) (var c)) (var b)), -- equivalence introduction
        imp (var b) (imp (var c) (var d)), -- left equivalence elimination
        imp (var b) (imp (var d) (var c)) -- right equivalence elimination
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

-- | Find an intuitionistic proof of a formula, if a proof exists.
--
-- The algorithm is due to
--
-- *  Richard Statman (1979)
--    "Intuitionistic propositional logic is polynomial-space complete."
--    Theoretical Computer Science, Volume 9, Issue 1, pp. 67–72.
--    <https://doi.org/10.1016/0304-3975(79)90006-9 DOI>.
proveTautology :: Ord a => Formula a -> Maybe (Proof a)
proveTautology a =
  -- Try proving a as an implicational formula first (this will obviously fail
  -- if a is not an implicational formula)
  (strengthenProof <$> proveImp ([] |- a))
    -- Next, try converting a into an implicational judgement and proving that.
    -- If this fails, then a is not a tautology.
    <|> (findSufficientSubproof <$> proveTautologyFromImp a)

-- The stuff below is a tedious conversion from an implicational proof
-- (obtained from proveImp . toImp) to a "regular" proof of the original formula

-- Convert a formula with extra variables x_a (as described in toImp) back to an
-- ordinary formula
fromImpFormula :: Formula (Formula a) -> Formula a
fromImpFormula (Lit b) = Lit b
fromImpFormula (Var a) = a
fromImpFormula (Imp _ _ a b) = imp (fromImpFormula a) (fromImpFormula b)
fromImpFormula _ = undefined -- should be unreachable!

-- Convert a judgement with extra variables x_a (as described in toImp) back to
-- an ordinary judgement
fromImpJudgement :: Ord a => Judgement (Formula a) -> Judgement a
fromImpJudgement (Judgement g a) = map fromImpFormula (Set.toList g) |- fromImpFormula a

-- Convert a proof with extra variables x_a (as described in toImp) back to an
-- ordinary proof
fromImpProof :: Ord a => Proof (Formula a) -> Proof a
fromImpProof (Ax j) = axiom (fromImpJudgement j)
fromImpProof (ImpElim _ j p q) = impElim (fromImpJudgement j) (fromImpProof p) (fromImpProof q)
fromImpProof (ImpIntr _ j p) = impIntr (fromImpJudgement j) (fromImpProof p)
-- This should be uncreachable! This function is intended to operate on proofs
-- constructed using proveImp, which will only use the axiom, implication
-- introduction, and implication elimination rules.
fromImpProof _ = undefined

-- Convert the formula to be proved to an implicational judgement as described
-- in toImp, and try proving the judgement. The resulting proof (if it exists)
-- will then be transformed into a proof of the original formula.
proveTautologyFromImp :: Ord a => Formula a -> Maybe (Proof a)
proveTautologyFromImp a = do
  -- Strengthening the proof here lets us remove some redundant hypotheses
  -- created by toImp.
  p <- strengthenProof <$> proveImp (toImp a)
  let g = antecedents (judgement p)
  return $ strengthenProof (foldl proveImpAxiom (fromImpProof p) g)
  where
    proveImpAxiom :: Ord a => Proof a -> Formula (Formula a) -> Proof a
    proveImpAxiom p b = subAxiom p (proveToImpHypothesis b)

    -- Substitute a proof for an axiom
    subAxiom p@(Ax (Judgement _ b)) q = if b == consequent (judgement q) then q else p
    subAxiom (FalseElim _ j p) q = falseElim j (subAxiom p q)
    subAxiom p@(TrueIntr _) _ = p
    subAxiom (NotElim _ j p q) r = notElim j (subAxiom p r) (subAxiom q r)
    subAxiom (NotIntr _ j p) q = notIntr j (subAxiom p q)
    subAxiom (ImpElim _ j p q) r = impElim j (subAxiom p r) (subAxiom q r)
    subAxiom (ImpIntr _ j p) q = impIntr j (subAxiom p q)
    subAxiom (OrElim _ j p q r) s = orElim j (subAxiom p s) (subAxiom q s) (subAxiom r s)
    subAxiom (OrIntrL _ j p) q = orIntrL j (subAxiom p q)
    subAxiom (OrIntrR _ j p) q = orIntrR j (subAxiom p q)
    subAxiom (AndElimL _ j p) q = andElimL j (subAxiom p q)
    subAxiom (AndElimR _ j p) q = andElimR j (subAxiom p q)
    subAxiom (AndIntr _ j p q) r = andIntr j (subAxiom p r) (subAxiom q r)
    subAxiom (IffElimL _ j p) q = iffElimL j (subAxiom p q)
    subAxiom (IffElimR _ j p) q = iffElimR j (subAxiom p q)
    subAxiom (IffIntr _ j p q) r = iffIntr j (subAxiom p r) (subAxiom q r)

-- toImp will produce a set of hypotheses, all of which are intuitionistic
-- tautologies. This function performs a pattern-matching-based conversion of
-- each hypothesis into a proof. Each pattern matching case corresponds to a
-- specific inference rule.
--
-- Recall from toImp that a variable Var a, where a itself is a propositional
-- formula, represents a new propositional variable x_a.
proveToImpHypothesis :: Ord a => Formula (Formula a) -> Proof a
-- Axiom
proveToImpHypothesis (Imp _ _ (Var a) (Var a'))
  | a == a' =
    impIntr
      ([] |- imp a a)
      (axiom ([a] |- a))
-- Falsity elimination
proveToImpHypothesis (Imp _ _ (Var (Lit False)) (Var b)) =
  impIntr
    ([] |- imp false b)
    ( falseElim
        ([false] |- b)
        ( axiom
            ( [false] |- false
            )
        )
    )
-- Vacuous truth introduction (special case)
proveToImpHypothesis (Var (Lit True)) = trueIntr ([] |- true)
-- Truth introduction
proveToImpHypothesis (Imp _ _ (Var b) (Var (Lit True))) =
  impIntr
    ([] |- imp b true)
    (trueIntr ([b] |- true))
-- Negation elimination
proveToImpHypothesis (Imp _ _ (Var (Not _ _ c)) (Imp _ _ (Var c') (Var (Lit False))))
  | c == c' =
    impIntr
      ([] |- imp (not c) (imp c false))
      ( impIntr
          ([not c] |- imp c false)
          ( notElim
              ([not c, c] |- false)
              (axiom ([not c] |- not c))
              (axiom ([c] |- c))
          )
      )
-- Negation introduction
proveToImpHypothesis (Imp _ _ (Imp _ _ (Var c) (Var (Lit False))) (Var (Not _ _ c')))
  | c == c' =
    impIntr
      ([] |- imp (imp c false) (not c))
      ( notIntr
          ([imp c false] |- not c)
          ( impElim
              ([c, imp c false] |- false)
              (axiom ([imp c false] |- imp c false))
              (axiom ([c] |- c))
          )
      )
-- Implication elimination
proveToImpHypothesis (Imp _ _ (Var (Imp _ _ c d)) (Imp _ _ (Var c') (Var d')))
  | c == c' && d == d' =
    impIntr
      ([] |- imp (imp c d) (imp c d))
      (axiom ([imp c d] |- imp c d))
-- Implication introduction
proveToImpHypothesis (Imp _ _ (Imp _ _ (Var c) (Var d)) (Var (Imp _ _ c' d')))
  | c == c' && d == d' =
    impIntr
      ([] |- imp (imp c d) (imp c d))
      (axiom ([imp c d] |- imp c d))
-- Left disjunction introduction
proveToImpHypothesis (Imp _ _ (Var c) (Var (Or _ _ c' d)))
  | c == c' =
    impIntr
      ([] |- imp c (or c d))
      ( orIntrL
          ([c] |- or c d)
          (axiom ([c] |- c))
      )
-- Right disjunction introduction
proveToImpHypothesis (Imp _ _ (Var d) (Var (Or _ _ c d')))
  | d == d' =
    impIntr
      ([] |- imp d (or c d))
      ( orIntrR
          ([d] |- or c d)
          (axiom ([d] |- d))
      )
-- Disjunction elimination
proveToImpHypothesis (Imp _ _ (Var (Or _ _ c d)) (Imp _ _ (Imp _ _ (Var c') (Var e)) (Imp _ _ (Imp _ _ (Var d') (Var e')) (Var e''))))
  | c == c' && d == d' && e == e' && e == e'' =
    impIntr
      ([] |- imp (or c d) (imp (imp c e) (imp (imp d e) e)))
      ( impIntr
          ([or c d] |- imp (imp c e) (imp (imp d e) e))
          ( impIntr
              ([or c d, imp c e] |- imp (imp d e) e)
              ( orElim
                  ([or c d, imp c e, imp d e] |- e)
                  (axiom ([or c d] |- or c d))
                  ( impElim
                      ([c, imp c e] |- e)
                      (axiom ([imp c e] |- imp c e))
                      (axiom ([c] |- c))
                  )
                  ( impElim
                      ([d, imp d e] |- e)
                      (axiom ([imp d e] |- imp d e))
                      (axiom ([d] |- d))
                  )
              )
          )
      )
-- Conjunction introduction
proveToImpHypothesis (Imp _ _ (Var c) (Imp _ _ (Var d) (Var (And _ _ c' d'))))
  | c == c' && d == d' =
    impIntr
      ([] |- imp c (imp d (and c d)))
      ( impIntr
          ([c] |- imp d (and c d))
          ( andIntr
              ([c, d] |- and c d)
              (axiom ([c] |- c))
              (axiom ([d] |- d))
          )
      )
-- Left conjunction elimination
proveToImpHypothesis (Imp _ _ (Var (And _ _ c d)) (Var c'))
  | c == c' =
    impIntr
      ([] |- imp (and c d) c)
      ( andElimL
          ([and c d] |- c)
          (axiom ([and c d] |- and c d))
      )
-- Right conjunction elimination
proveToImpHypothesis (Imp _ _ (Var (And _ _ c d)) (Var d'))
  | d == d' =
    impIntr
      ([] |- imp (and c d) d)
      ( andElimR
          ([and c d] |- d)
          (axiom ([and c d] |- and c d))
      )
-- Equivalence introduction
proveToImpHypothesis (Imp _ _ (Imp _ _ (Var c) (Var d)) (Imp _ _ (Imp _ _ (Var c') (Var d')) (Var (Iff _ _ c'' d''))))
  | c == c' && c == c'' && d == d' && d == d'' =
    impIntr
      ([] |- imp (imp c d) (imp (imp d c) (iff c d)))
      ( impIntr
          ([imp c d] |- imp (imp d c) (iff c d))
          ( iffIntr
              ([imp c d, imp d c] |- iff c d)
              (axiom ([imp c d] |- imp c d))
              (axiom ([imp d c] |- imp d c))
          )
      )
-- Left equivalence elimination
proveToImpHypothesis (Imp _ _ (Var (Iff _ _ c d)) (Imp _ _ (Var c') (Var d')))
  | c == c' && d == d' =
    impIntr
      ([] |- imp (iff c d) (imp c d))
      ( iffElimL
          ([iff c d] |- imp c d)
          (axiom ([iff c d] |- iff c d))
      )
-- Right equivalence elimination
proveToImpHypothesis (Imp _ _ (Var (Iff _ _ c d)) (Imp _ _ (Var d') (Var c')))
  | c == c' && d == d' =
    impIntr
      ([] |- imp (iff c d) (imp d c))
      ( iffElimL
          ([iff c d] |- imp d c)
          (axiom ([iff c d] |- iff c d))
      )
-- This should never be reached! If it is, that means we're mishandling one of
-- the cases in the toImp translation
proveToImpHypothesis _ = undefined

-- Take a proof and return a subproof (maybe the original proof itself) that
-- proves the judgment proved by the original proof. For example, suppose we're
-- proving ⊢ (a ∧ a) → a. The following proof works:
--
--                                         ------------- (Ax)
--                                         a ∧ a ⊢ a ∧ a
--    ------------------------- (Ax)       ------------- (∧EL)
--    (a ∧ a) → a ⊢ (a ∧ a) → a              a ∧ a ⊢ a
-- ------------------------------- (→I)    ------------- (→I)
-- ⊢ ((a ∧ a) → a) → ((a ∧ a) → a)         ⊢ (a ∧ a) → a
-- ----------------------------------------------------- (→E)
--                       ⊢ (a ∧ a) → a
--
-- However, the right branch of the root inference is already a proof! This
-- function finds such subproofs.
findSufficientSubproof :: Ord a => Proof a -> Proof a
findSufficientSubproof p = fromMaybe p (f p)
  where
    j = judgement p
    f q = foldr ((<|>) . f) (g q) (sort (premises q))
    g q = if judgement q == j then Just q else Nothing
