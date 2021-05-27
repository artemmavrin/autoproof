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

import AutoProof.AST (AST (children, root))
import AutoProof.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    subformulas,
  )
import AutoProof.Judgement
  ( Judgement
      ( Judgement,
        antecedents,
        succedent
      ),
    (|-),
  )
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
    notElim,
    notIntr,
    orElim,
    orIntrL,
    orIntrR,
    trueIntr,
  )
import AutoProof.Utils.DList (fromDList, toDList)
import Control.Applicative (Alternative ((<|>)))
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set

-- | Convert a propositional formula into an implicational judgement which is
-- provable if And only if the original formula is an intuitionistic tautology.
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
-- propositional logic such that \(a\) is an intuitionistic tautology if And
-- only if the judgement \(g \vdash a^*\) is provable. This function computes
-- the translation from \(a\) to \(g \vdash a^*\), enabling the checking of
-- whether \(a\) is a tautology by checking the provability of \(g \vdash a^*\)
-- (using, e.g., 'AutoProof.Proof.Implication.proveImp').
--
-- The transformation from \(a\) to \(g \vdash a^*\) involves two steps:
--
-- 1. For each propositional formula \(b\), introduce a Var propositional
--    variable \(x_b\). Take \(a^* = x_a\).
-- 2. Let \(g\) be the set consisting of formulas of the following form:
--
--     1. (truth)
--        \[
--          x_\top
--        \]
--     2. (truth introduction And falsity elimination) for each subformula \(b\)
--        of \(a\),
--        \[
--          x_\bot \rightarrow x_b, \qquad
--          x_b \rightarrow x_\top
--        \]
--     3. (negation introduction And elimination) for each subformula \(b\) of
--        \(a\) of the form \(b = \neg c\),
--        \[
--          (x_c \rightarrow x_\bot) \rightarrow x_b, \qquad
--          x_b \rightarrow (x_c \rightarrow x_\bot)
--        \]
--     4. (implication introduction And elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \rightarrow d\),
--        \[
--          (x_c \rightarrow x_d) \rightarrow x_b, \qquad
--          x_b \rightarrow (x_c \rightarrow x_d)
--        \]
--     5. (disjunction introduction And elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \lor d\), And each additional subformula
--        \(e\) of \(a\),
--        \[
--          x_c \rightarrow x_b, \qquad
--          x_d \rightarrow x_b, \qquad
--          x_b \rightarrow ((x_c \rightarrow x_e) \rightarrow ((x_d \rightarrow x_e) \rightarrow x_e))
--        \]
--     6. (conjunction introduction And elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \land d\),
--        \[
--          x_c \rightarrow (x_d \rightarrow x_b), \qquad
--          x_b \rightarrow x_c, \qquad
--          x_b \rightarrow x_d
--        \]
--     7. (equivalence introduction And elimination) for each subformula \(b\)
--        of \(a\) of the form \(b = c \leftrightarrow d\),
--        \[
--          (x_c \rightarrow x_d) \rightarrow ((x_d \rightarrow x_c) \rightarrow x_b), \qquad
--          x_b \rightarrow (x_c \rightarrow x_d), \qquad
--          x_b \rightarrow (x_d \rightarrow x_c)
--        \]
toImp :: Ord a => Formula a -> Judgement (Formula a)
toImp a = Judgement g (Var a)
  where
    bs = subformulas a

    -- Final context, created by recursing over subformulas of a
    g = Set.fromList $ fromDList $ f g0 a

    -- Add common formulas into the growing context g'
    f g' b = h (g' . common b) b

    -- Add unique formulas into the growing context g'
    h g' b@(Lit _) = g' . unique b
    h g' b@(Var _) = g' . unique b
    h g' b@(Not c) = f (g' . unique b) c
    h g' b@(Imp c d) = f (f (g' . unique b) c) d
    h g' b@(Or c d) = f (f (g' . unique b) c) d
    h g' b@(And c d) = f (f (g' . unique b) c) d
    h g' b@(Iff c d) = f (f (g' . unique b) c) d

    -- Initial context (vacuous truth introduction)
    g0 = toDList [Var (Lit True)]

    -- Formulas that get created for every subformula b of a
    common b =
      toDList
        [ Imp (Var (Lit False)) (Var b), -- falsity elimination
          Imp (Var b) (Var (Lit True)) -- truth introduction
        ]

    -- Formulas that get created depending on the shape of a subformula b of a
    unique = toDList . unique'
    unique' (Lit _) = []
    unique' (Var _) = []
    unique' b@(Not c) =
      [ Imp (Var b) (Imp (Var c) (Var (Lit False))), -- negation elimination
        Imp (Imp (Var c) (Var (Lit False))) (Var b) -- negation introduction
      ]
    unique' b@(Imp c d) =
      [ Imp (Var b) (Imp (Var c) (Var d)), -- implication elimination
        Imp (Imp (Var c) (Var d)) (Var b) -- implication introduction
      ]
    unique' b@(Or c d) =
      let t0 =
            [ Imp (Var c) (Var b), -- left disjunction introduction
              Imp (Var d) (Var b) -- right disjunction introduction
            ]
          -- disjunction elimination
          orE e = Imp (Var b) (Imp (Imp (Var c) (Var e)) (Imp (Imp (Var d) (Var e)) (Var e)))
       in foldr ((:) . orE) t0 bs
    unique' b@(And c d) =
      [ Imp (Var c) (Imp (Var d) (Var b)), -- conjunction introduction
        Imp (Var b) (Var c), -- left conjunction elimination
        Imp (Var b) (Var d) -- right conjunction elimination
      ]
    unique' b@(Iff c d) =
      [ Imp (Imp (Var c) (Var d)) (Imp (Imp (Var d) (Var c)) (Var b)), -- equivalence introduction
        Imp (Var b) (Imp (Var c) (Var d)), -- left equivalence elimination
        Imp (Var b) (Imp (Var d) (Var c)) -- right equivalence elimination
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
-- >>> isTautology $ Imp (And (Var 'a') (Var 'b')) (Var 'a')
-- True
--
-- >>> isTautology $ Or (Var 'a') (Not (Var 'a'))
-- False
--
-- >>> isTautology $ Not (Not (Or (Var 'a') (Not (Var 'a'))))
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
  -- if a is Not an implicational formula)
  (strengthenProof <$> proveImp ([] |- a))
    -- Next, try converting a into an implicational judgement And proving that.
    -- If this fails, then a is Not a tautology.
    <|> (findSufficientSubproof <$> proveTautologyFromImp a)

-- The stuff below is a tedious conversion from an implicational proof
-- (obtained from proveImp . toImp) to a "regular" proof of the original formula

-- Convert a formula with extra variables x_a (as described in toImp) back to an
-- ordinary formula
fromImpFormula :: Formula (Formula a) -> Formula a
fromImpFormula (Lit b) = Lit b
fromImpFormula (Var a) = a
fromImpFormula (Imp a b) = Imp (fromImpFormula a) (fromImpFormula b)
fromImpFormula _ = undefined -- should be unreachable!

-- Convert a judgement with extra variables x_a (as described in toImp) back to
-- an ordinary judgement
fromImpJudgement :: Ord a => Judgement (Formula a) -> Judgement a
fromImpJudgement (Judgement g a) = map fromImpFormula (Set.toList g) |- fromImpFormula a

-- Convert a proof with extra variables x_a (as described in toImp) back to an
-- ordinary proof
fromImpProof :: Ord a => Proof (Formula a) -> Proof a
fromImpProof (Ax _ j) = axiom (fromImpJudgement j)
fromImpProof (ImpElim _ j p q) = impElim (fromImpJudgement j) (fromImpProof p) (fromImpProof q)
fromImpProof (ImpIntr _ j p) = impIntr (fromImpJudgement j) (fromImpProof p)
-- This should be uncreachable! This function is intended to operate on proofs
-- constructed using proveImp, which will only use the axiom, implication
-- introduction, And implication elimination rules.
fromImpProof _ = undefined

-- Convert the formula to be proved to an implicational judgement as described
-- in toImp, And try proving the judgement. The resulting proof (if it exists)
-- will then be transformed into a proof of the original formula.
proveTautologyFromImp :: Ord a => Formula a -> Maybe (Proof a)
proveTautologyFromImp a = do
  -- Strengthening the proof here lets us remove some redundant hypotheses
  -- created by toImp.
  p <- strengthenProof <$> proveImp (toImp a)
  let g = antecedents (root p)
  return $ strengthenProof (foldl proveImpAxiom (fromImpProof p) g)
  where
    proveImpAxiom p b = subAxiom p (proveToImpHypothesis b)

    -- Substitute a proof for an axiom
    subAxiom p@(Ax _ (Judgement _ b)) q = if b == succedent (root q) then q else p
    subAxiom (FalseElim _ j p) q = falseElim j (subAxiom p q)
    subAxiom p@(TrueIntr _ _) _ = p
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
proveToImpHypothesis (Imp (Var a) (Var a'))
  | a == a' =
    impIntr
      ([] |- Imp a a)
      (axiom ([a] |- a))
-- Falsity elimination
proveToImpHypothesis (Imp (Var (Lit False)) (Var b)) =
  impIntr
    ([] |- Imp (Lit False) b)
    ( falseElim
        ([(Lit False)] |- b)
        ( axiom
            ( [(Lit False)] |- (Lit False)
            )
        )
    )
-- Vacuous truth introduction (special case)
proveToImpHypothesis (Var (Lit True)) = trueIntr ([] |- (Lit True))
-- Truth introduction
proveToImpHypothesis (Imp (Var b) (Var (Lit True))) =
  impIntr
    ([] |- Imp b (Lit True))
    (trueIntr ([b] |- (Lit True)))
-- Negation elimination
proveToImpHypothesis (Imp (Var (Not c)) (Imp (Var c') (Var (Lit False))))
  | c == c' =
    impIntr
      ([] |- Imp (Not c) (Imp c (Lit False)))
      ( impIntr
          ([Not c] |- Imp c (Lit False))
          ( notElim
              ([Not c, c] |- (Lit False))
              (axiom ([Not c] |- Not c))
              (axiom ([c] |- c))
          )
      )
-- Negation introduction
proveToImpHypothesis (Imp (Imp (Var c) (Var (Lit False))) (Var (Not c')))
  | c == c' =
    impIntr
      ([] |- Imp (Imp c (Lit False)) (Not c))
      ( notIntr
          ([Imp c (Lit False)] |- Not c)
          ( impElim
              ([c, Imp c (Lit False)] |- (Lit False))
              (axiom ([Imp c (Lit False)] |- Imp c (Lit False)))
              (axiom ([c] |- c))
          )
      )
-- Implication elimination
proveToImpHypothesis (Imp (Var (Imp c d)) (Imp (Var c') (Var d')))
  | c == c' && d == d' =
    impIntr
      ([] |- Imp (Imp c d) (Imp c d))
      (axiom ([Imp c d] |- Imp c d))
-- Implication introduction
proveToImpHypothesis (Imp (Imp (Var c) (Var d)) (Var (Imp c' d')))
  | c == c' && d == d' =
    impIntr
      ([] |- Imp (Imp c d) (Imp c d))
      (axiom ([Imp c d] |- Imp c d))
-- Left disjunction introduction
proveToImpHypothesis (Imp (Var c) (Var (Or c' d)))
  | c == c' =
    impIntr
      ([] |- Imp c (Or c d))
      ( orIntrL
          ([c] |- Or c d)
          (axiom ([c] |- c))
      )
-- Right disjunction introduction
proveToImpHypothesis (Imp (Var d) (Var (Or c d')))
  | d == d' =
    impIntr
      ([] |- Imp d (Or c d))
      ( orIntrR
          ([d] |- Or c d)
          (axiom ([d] |- d))
      )
-- Disjunction elimination
proveToImpHypothesis (Imp (Var (Or c d)) (Imp (Imp (Var c') (Var e)) (Imp (Imp (Var d') (Var e')) (Var e''))))
  | c == c' && d == d' && e == e' && e == e'' =
    impIntr
      ([] |- Imp (Or c d) (Imp (Imp c e) (Imp (Imp d e) e)))
      ( impIntr
          ([Or c d] |- Imp (Imp c e) (Imp (Imp d e) e))
          ( impIntr
              ([Or c d, Imp c e] |- Imp (Imp d e) e)
              ( orElim
                  ([Or c d, Imp c e, Imp d e] |- e)
                  (axiom ([Or c d] |- Or c d))
                  ( impElim
                      ([c, Imp c e] |- e)
                      (axiom ([Imp c e] |- Imp c e))
                      (axiom ([c] |- c))
                  )
                  ( impElim
                      ([d, Imp d e] |- e)
                      (axiom ([Imp d e] |- Imp d e))
                      (axiom ([d] |- d))
                  )
              )
          )
      )
-- Conjunction introduction
proveToImpHypothesis (Imp (Var c) (Imp (Var d) (Var (And c' d'))))
  | c == c' && d == d' =
    impIntr
      ([] |- Imp c (Imp d (And c d)))
      ( impIntr
          ([c] |- Imp d (And c d))
          ( andIntr
              ([c, d] |- And c d)
              (axiom ([c] |- c))
              (axiom ([d] |- d))
          )
      )
-- Left conjunction elimination
proveToImpHypothesis (Imp (Var (And c d)) (Var c'))
  | c == c' =
    impIntr
      ([] |- Imp (And c d) c)
      ( andElimL
          ([And c d] |- c)
          (axiom ([And c d] |- And c d))
      )
-- Right conjunction elimination
proveToImpHypothesis (Imp (Var (And c d)) (Var d'))
  | d == d' =
    impIntr
      ([] |- Imp (And c d) d)
      ( andElimR
          ([And c d] |- d)
          (axiom ([And c d] |- And c d))
      )
-- Equivalence introduction
proveToImpHypothesis (Imp (Imp (Var c) (Var d)) (Imp (Imp (Var d') (Var c')) (Var (Iff c'' d''))))
  | c == c' && c == c'' && d == d' && d == d'' =
    impIntr
      ([] |- Imp (Imp c d) (Imp (Imp d c) (Iff c d)))
      ( impIntr
          ([Imp c d] |- Imp (Imp d c) (Iff c d))
          ( iffIntr
              ([Imp c d, Imp d c] |- Iff c d)
              (axiom ([Imp c d] |- Imp c d))
              (axiom ([Imp d c] |- Imp d c))
          )
      )
-- Left equivalence elimination
proveToImpHypothesis (Imp (Var (Iff c d)) (Imp (Var c') (Var d')))
  | c == c' && d == d' =
    impIntr
      ([] |- Imp (Iff c d) (Imp c d))
      ( iffElimL
          ([Iff c d] |- Imp c d)
          (axiom ([Iff c d] |- Iff c d))
      )
-- Right equivalence elimination
proveToImpHypothesis (Imp (Var (Iff c d)) (Imp (Var d') (Var c')))
  | c == c' && d == d' =
    impIntr
      ([] |- Imp (Iff c d) (Imp d c))
      ( iffElimR
          ([Iff c d] |- Imp d c)
          (axiom ([Iff c d] |- Iff c d))
      )
-- This should never be reached! If it is, that means we're mishandling one of
-- the cases in the toImp translation
proveToImpHypothesis _ = undefined

-- Take a proof And return a subproof (maybe the original proof itself) that
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
    j = root p
    f q = foldr ((<|>) . f) (g q) (sort (children q))
    g q = if root q == j then Just q else Nothing
