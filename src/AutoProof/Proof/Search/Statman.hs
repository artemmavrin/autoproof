-- |
-- Module      : AutoProof.Proof.Search.Statman
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Check provability of general propositional formulas using an algorithm
-- derived from
--
-- *  Richard Statman (1979)
--    "Intuitionistic propositional logic is polynomial-space complete."
--    Theoretical Computer Science, Volume 9, Issue 1, pp. 67–72.
--    <https://doi.org/10.1016/0304-3975(79)90006-9 DOI>.
module AutoProof.Proof.Search.Statman
  ( proveStatman,
    toImp,
  )
where

import AutoProof.AST (AST (root))
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
import AutoProof.Proof.Search.Implication (proveImp)
import AutoProof.Proof.Transform (strengthenProof)
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
import qualified AutoProof.Utils.DList as DList (cons, empty, fromList, toSet)
import qualified Data.Set as Set

-- | Convert a general propositional judgement into an implicational judgement
-- which is intuitionistically provable if and only if the original judgement is
-- intuitionistically provable.
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
-- This algorithm turns a judgement \(g \vdash a\) in full propositional logic
-- into a judgement \(g^* \vdash a^*\) in just the implicational fragment of
-- propositional logic such that \(g \vdash a\) is provable if and only if the
-- judgement \(g^* \vdash a^*\) is provable. The latter judgement's provability
-- can then be checked using 'AutoProof.Proof.Search.Implication.proveImp'.
--
-- The transformation from \(g \vdash a\) to \(g^* \vdash a^*\) involves two
-- steps:
--
-- 1. For each propositional formula \(b\), introduce a new propositional
--    variable \(x_b\). Take \(a^* = x_a\).
-- 2. Construct the set \(g^*\) as follows.
--
--     1. For each formula \(b \in g\), add \(x_b\) to \(g^*\).
--     2. (Truth)
--        Add \(x_\top\) to  \(g^*\).
--     3. For each subformula \(b\) of either \(a\) or a formula in \(g\), do
--        the following.
--
--         1. (Falsity elimination and truth introduction)
--            Add the following formulas to \(g^*\).
--            \[
--              \begin{gathered}
--                x_\bot \rightarrow x_b \\
--                x_b \rightarrow x_\top
--              \end{gathered}
--            \]
--         2. (Negation introduction and elimination)
--            If \(b = \lnot c\), then add the following formulas to \(g^*\).
--            \[
--              \begin{gathered}
--                (x_c \rightarrow x_\bot) \rightarrow x_{\lnot c} \\
--                x_{\lnot c} \rightarrow (x_c \rightarrow x_\bot)
--              \end{gathered}
--            \]
--         3. (Implication introduction and elimination)
--            If \(b = c \rightarrow d\), then add the following formulas to
--            \(g^*\).
--            \[
--              \begin{gathered}
--                (x_c \rightarrow x_d) \rightarrow x_{c \rightarrow d} \\
--                x_{c \rightarrow d} \rightarrow (x_c \rightarrow x_d)
--              \end{gathered}
--            \]
--         4. (Disjunction introduction and elimination)
--            If \(b = c \lor d\), then for each additional subformula \(e\) of
--            \(a\), add the following formulas to \(g^*\).
--            \[
--              \begin{gathered}
--                x_c \rightarrow x_{c \lor d} \\
--                x_d \rightarrow x_{c \lor d} \\
--                x_{c \lor d} \rightarrow ((x_c \rightarrow x_e) \rightarrow ((x_d \rightarrow x_e) \rightarrow x_e))
--              \end{gathered}
--            \]
--         5. (Conjunction introduction and elimination)
--            If \(b = c \land d\), then add the following formulas to \(g^*\).
--            \[
--              \begin{gathered}
--                x_c \rightarrow (x_d \rightarrow x_{c \land d}) \\
--                x_{c \land d} \rightarrow x_c \\
--                x_{c \land d} \rightarrow x_d
--              \end{gathered}
--            \]
--         6. (Equivalence introduction and elimination)
--            If \(b = c \leftrightarrow d\), then add the following formulas to
--            \(g^*\).
--            \[
--              \begin{gathered}
--                (x_c \rightarrow x_d) \rightarrow ((x_d \rightarrow x_c) \rightarrow x_{c \leftrightarrow d}) \\
--                x_{c \leftrightarrow d} \rightarrow (x_c \rightarrow x_d) \\
--                x_{c \leftrightarrow d} \rightarrow (x_d \rightarrow x_c)
--              \end{gathered}
--            \]
toImp :: Ord a => Judgement a -> Judgement (Formula a)
toImp (Judgement g a) = Judgement gStar (Var a)
  where
    -- Initial implicational context (vacuous truth introduction)
    g0 = DList.fromList [Var (Lit True)]

    -- Intermediate implicational context: add all formulas derived from the
    -- succedent a
    g1 = addFormulas g0 a

    -- Final implicational context g^*, created by recursing over subformulas of
    -- formulas in the original context g
    gStar = DList.toSet $ foldl addFormulas' g1 g

    -- Add formulas derived from a given formula b into the growing context g'
    addFormulas g' b = addFormulasByPattern (g' . common b) b

    -- Add formulas derived from a given formula b into the growing context g',
    -- but also add x_b (the new propositional variable obtained from b)
    addFormulas' g' b = DList.cons (Var b) (addFormulas g' b)

    -- Add formulas that depend on the form of b into the growing context g'
    addFormulasByPattern g' b@(Lit _) = g' . unique b
    addFormulasByPattern g' b@(Var _) = g' . unique b
    addFormulasByPattern g' b@(Not c) = addFormulas (g' . unique b) c
    addFormulasByPattern g' b@(Imp c d) = addFormulas (addFormulas (g' . unique b) c) d
    addFormulasByPattern g' b@(Or c d) = addFormulas (addFormulas (g' . unique b) c) d
    addFormulasByPattern g' b@(And c d) = addFormulas (addFormulas (g' . unique b) c) d
    addFormulasByPattern g' b@(Iff c d) = addFormulas (addFormulas (g' . unique b) c) d

    -- Formulas that get created for every subformula b
    common b =
      DList.fromList
        [ Imp (Var (Lit False)) (Var b), -- Falsity elimination
          Imp (Var b) (Var (Lit True)) -- Truth introduction
        ]

    -- Formulas that get created depending on the form of a subformula b
    unique (Lit _) = DList.empty
    unique (Var _) = DList.empty
    unique b@(Not c) =
      DList.fromList
        [ Imp (Var b) (Imp (Var c) (Var (Lit False))), -- Negation elimination
          Imp (Imp (Var c) (Var (Lit False))) (Var b) -- Negation introduction
        ]
    unique b@(Imp c d) =
      DList.fromList
        [ Imp (Var b) (Imp (Var c) (Var d)), -- Implication elimination
          Imp (Imp (Var c) (Var d)) (Var b) -- Implication introduction
        ]
    unique b@(Or c d) =
      DList.fromList $
        let introductions =
              [ Imp (Var c) (Var b), -- Left disjunction introduction
                Imp (Var d) (Var b) -- Right disjunction introduction
              ]
            -- Disjunction elimination schema
            elimination e = Imp (Var b) (Imp (Imp (Var c) (Var e)) (Imp (Imp (Var d) (Var e)) (Var e)))
         in foldr ((:) . elimination) introductions (subformulas a)
    -- TODO: is it sufficient to consider only subformulas of a above?
    unique b@(And c d) =
      DList.fromList
        [ Imp (Var c) (Imp (Var d) (Var b)), -- Conjunction introduction
          Imp (Var b) (Var c), -- Left conjunction elimination
          Imp (Var b) (Var d) -- Right conjunction elimination
        ]
    unique b@(Iff c d) =
      DList.fromList
        [ Imp (Imp (Var c) (Var d)) (Imp (Imp (Var d) (Var c)) (Var b)), -- Equivalence introduction
          Imp (Var b) (Imp (Var c) (Var d)), -- Left equivalence elimination
          Imp (Var b) (Imp (Var d) (Var c)) -- Right equivalence elimination
        ]

-- | Find an intuitionistic proof of a judgement, if one exists, using Statman's
-- algorithm.
--
-- *  Richard Statman (1979)
--    "Intuitionistic propositional logic is polynomial-space complete."
--    Theoretical Computer Science, Volume 9, Issue 1, pp. 67–72.
--    <https://doi.org/10.1016/0304-3975(79)90006-9 DOI>.
--
-- The judgement to be proved is converted to an implicational judgement as
-- described in 'toImp' and proved (if possible) using 'proveImp'. The resulting
-- proof is then converted into a proof of the original judgement.
proveStatman :: Ord a => Judgement a -> Maybe (Proof a)
proveStatman j@(Judgement g _) = do
  -- Strengthening the proof here lets us remove some redundant hypotheses
  -- created by toImp.
  p <- strengthenProof <$> proveImp (toImp j)
  let g' = antecedents (root p)
  let proveImpAxiom q (Var c) | c `Set.member` g = q
      proveImpAxiom q b = subAxiom q (proveToImpHypothesis b)
  return (strengthenProof (foldl proveImpAxiom (fromImpProof p) g'))
  where
    -- Substitute a proof for an axiom
    subAxiom p@(Axiom (Judgement _ b)) q = if b == succedent (root q) then q else p
    subAxiom (FalseElim j' p) q = FalseElim j' (subAxiom p q)
    subAxiom p@(TrueIntr _) _ = p
    subAxiom (NotElim j' p q) r = NotElim j' (subAxiom p r) (subAxiom q r)
    subAxiom (NotIntr j' p) q = NotIntr j' (subAxiom p q)
    subAxiom (ImpElim j' p q) r = ImpElim j' (subAxiom p r) (subAxiom q r)
    subAxiom (ImpIntr j' p) q = ImpIntr j' (subAxiom p q)
    subAxiom (OrElim j' p q r) s = OrElim j' (subAxiom p s) (subAxiom q s) (subAxiom r s)
    subAxiom (OrIntrL j' p) q = OrIntrL j' (subAxiom p q)
    subAxiom (OrIntrR j' p) q = OrIntrR j' (subAxiom p q)
    subAxiom (AndElimL j' p) q = AndElimL j' (subAxiom p q)
    subAxiom (AndElimR j' p) q = AndElimR j' (subAxiom p q)
    subAxiom (AndIntr j' p q) r = AndIntr j' (subAxiom p r) (subAxiom q r)
    subAxiom (IffElimL j' p) q = IffElimL j' (subAxiom p q)
    subAxiom (IffElimR j' p) q = IffElimR j' (subAxiom p q)
    subAxiom (IffIntr j' p q) r = IffIntr j' (subAxiom p r) (subAxiom q r)

-- The stuff below is a tedious conversion from an implicational proof
-- (obtained from proveImp . toImp) to a "regular" proof of the original
-- judgement

-- Convert a formula with extra variables x_a (as described in toImp) back to an
-- ordinary formula
fromImpFormula :: Formula (Formula a) -> Formula a
fromImpFormula (Lit b) = Lit b
fromImpFormula (Var a) = a
fromImpFormula (Imp a b) = Imp (fromImpFormula a) (fromImpFormula b)
-- This should be unreachable in intended use cases.
fromImpFormula _ = undefined

-- Convert a judgement with extra variables x_a (as described in toImp) back to
-- an ordinary judgement
fromImpJudgement :: Ord a => Judgement (Formula a) -> Judgement a
fromImpJudgement (Judgement g a) = map fromImpFormula (Set.toList g) |- fromImpFormula a

-- Convert a proof with extra variables x_a (as described in toImp) back to an
-- ordinary proof
fromImpProof :: Ord a => Proof (Formula a) -> Proof a
fromImpProof (Axiom j) = Axiom (fromImpJudgement j)
fromImpProof (ImpElim j p q) = ImpElim (fromImpJudgement j) (fromImpProof p) (fromImpProof q)
fromImpProof (ImpIntr j p) = ImpIntr (fromImpJudgement j) (fromImpProof p)
-- This should be uncreachable! This function is intended to operate on proofs
-- constructed using proveImp, which will only use the axiom, implication
-- introduction, And implication elimination rules.
fromImpProof _ = undefined

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
    ImpIntr
      ([] |- Imp a a)
      (Axiom ([a] |- a))
-- Falsity elimination
proveToImpHypothesis (Imp (Var (Lit False)) (Var b)) =
  ImpIntr
    ([] |- Imp (Lit False) b)
    ( FalseElim
        ([Lit False] |- b)
        ( Axiom
            ( [Lit False] |- Lit False
            )
        )
    )
-- Vacuous truth introduction (special case)
proveToImpHypothesis (Var (Lit True)) = TrueIntr ([] |- Lit True)
-- Truth introduction
proveToImpHypothesis (Imp (Var b) (Var (Lit True))) =
  ImpIntr
    ([] |- Imp b (Lit True))
    (TrueIntr ([b] |- Lit True))
-- Negation elimination
proveToImpHypothesis (Imp (Var (Not c)) (Imp (Var c') (Var (Lit False))))
  | c == c' =
    ImpIntr
      ([] |- Imp (Not c) (Imp c (Lit False)))
      ( ImpIntr
          ([Not c] |- Imp c (Lit False))
          ( NotElim
              ([Not c, c] |- Lit False)
              (Axiom ([Not c] |- Not c))
              (Axiom ([c] |- c))
          )
      )
-- Negation introduction
proveToImpHypothesis (Imp (Imp (Var c) (Var (Lit False))) (Var (Not c')))
  | c == c' =
    ImpIntr
      ([] |- Imp (Imp c (Lit False)) (Not c))
      ( NotIntr
          ([Imp c (Lit False)] |- Not c)
          ( ImpElim
              ([c, Imp c (Lit False)] |- Lit False)
              (Axiom ([Imp c (Lit False)] |- Imp c (Lit False)))
              (Axiom ([c] |- c))
          )
      )
-- Implication elimination
proveToImpHypothesis (Imp (Var (Imp c d)) (Imp (Var c') (Var d')))
  | c == c' && d == d' =
    ImpIntr
      ([] |- Imp (Imp c d) (Imp c d))
      (Axiom ([Imp c d] |- Imp c d))
-- Implication introduction
proveToImpHypothesis (Imp (Imp (Var c) (Var d)) (Var (Imp c' d')))
  | c == c' && d == d' =
    ImpIntr
      ([] |- Imp (Imp c d) (Imp c d))
      (Axiom ([Imp c d] |- Imp c d))
-- Left disjunction introduction
proveToImpHypothesis (Imp (Var c) (Var (Or c' d)))
  | c == c' =
    ImpIntr
      ([] |- Imp c (Or c d))
      ( OrIntrL
          ([c] |- Or c d)
          (Axiom ([c] |- c))
      )
-- Right disjunction introduction
proveToImpHypothesis (Imp (Var d) (Var (Or c d')))
  | d == d' =
    ImpIntr
      ([] |- Imp d (Or c d))
      ( OrIntrR
          ([d] |- Or c d)
          (Axiom ([d] |- d))
      )
-- Disjunction elimination
proveToImpHypothesis (Imp (Var (Or c d)) (Imp (Imp (Var c') (Var e)) (Imp (Imp (Var d') (Var e')) (Var e''))))
  | c == c' && d == d' && e == e' && e == e'' =
    ImpIntr
      ([] |- Imp (Or c d) (Imp (Imp c e) (Imp (Imp d e) e)))
      ( ImpIntr
          ([Or c d] |- Imp (Imp c e) (Imp (Imp d e) e))
          ( ImpIntr
              ([Or c d, Imp c e] |- Imp (Imp d e) e)
              ( OrElim
                  ([Or c d, Imp c e, Imp d e] |- e)
                  (Axiom ([Or c d] |- Or c d))
                  ( ImpElim
                      ([c, Imp c e] |- e)
                      (Axiom ([Imp c e] |- Imp c e))
                      (Axiom ([c] |- c))
                  )
                  ( ImpElim
                      ([d, Imp d e] |- e)
                      (Axiom ([Imp d e] |- Imp d e))
                      (Axiom ([d] |- d))
                  )
              )
          )
      )
-- Conjunction introduction
proveToImpHypothesis (Imp (Var c) (Imp (Var d) (Var (And c' d'))))
  | c == c' && d == d' =
    ImpIntr
      ([] |- Imp c (Imp d (And c d)))
      ( ImpIntr
          ([c] |- Imp d (And c d))
          ( AndIntr
              ([c, d] |- And c d)
              (Axiom ([c] |- c))
              (Axiom ([d] |- d))
          )
      )
-- Left conjunction elimination
proveToImpHypothesis (Imp (Var (And c d)) (Var c'))
  | c == c' =
    ImpIntr
      ([] |- Imp (And c d) c)
      ( AndElimL
          ([And c d] |- c)
          (Axiom ([And c d] |- And c d))
      )
-- Right conjunction elimination
proveToImpHypothesis (Imp (Var (And c d)) (Var d'))
  | d == d' =
    ImpIntr
      ([] |- Imp (And c d) d)
      ( AndElimR
          ([And c d] |- d)
          (Axiom ([And c d] |- And c d))
      )
-- Equivalence introduction
proveToImpHypothesis (Imp (Imp (Var c) (Var d)) (Imp (Imp (Var d') (Var c')) (Var (Iff c'' d''))))
  | c == c' && c == c'' && d == d' && d == d'' =
    ImpIntr
      ([] |- Imp (Imp c d) (Imp (Imp d c) (Iff c d)))
      ( ImpIntr
          ([Imp c d] |- Imp (Imp d c) (Iff c d))
          ( IffIntr
              ([Imp c d, Imp d c] |- Iff c d)
              (Axiom ([Imp c d] |- Imp c d))
              (Axiom ([Imp d c] |- Imp d c))
          )
      )
-- Left equivalence elimination
proveToImpHypothesis (Imp (Var (Iff c d)) (Imp (Var c') (Var d')))
  | c == c' && d == d' =
    ImpIntr
      ([] |- Imp (Iff c d) (Imp c d))
      ( IffElimL
          ([Iff c d] |- Imp c d)
          (Axiom ([Iff c d] |- Iff c d))
      )
-- Right equivalence elimination
proveToImpHypothesis (Imp (Var (Iff c d)) (Imp (Var d') (Var c')))
  | c == c' && d == d' =
    ImpIntr
      ([] |- Imp (Iff c d) (Imp d c))
      ( IffElimR
          ([Iff c d] |- Imp d c)
          (Axiom ([Iff c d] |- Iff c d))
      )
-- This should never be reached! If it is, that means we're mishandling one of
-- the cases in the toImp translation
proveToImpHypothesis _ = undefined
