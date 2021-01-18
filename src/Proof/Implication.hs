-- |
-- Module      : Proof.Implication
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic proofs in the implicational fragment of propositional logic.
module Proof.Implication
  ( -- * Type definitions
    Formula (Var, Imp),
    Context,
    Proof (Ax, ImpElim, ImpIntr),

    -- * Proof search
    prove,

    -- * Debugging
    checkProof,
  )
where

-- | Formulas in the implicational fragment of propositional logic
data Formula a
  = -- | @(Var x)@ represents a propositional variable @x@.
    Var a
  | -- | @(Imp a b)@ represents the implication @a → b@ between propositional
    -- formulas @a@ and @b@.
    Imp (Formula a) (Formula a)
  deriving (Eq, Ord, Show)

-- | A sequence of propositional formulas.
type Context a = [Formula a]

-- | An intuitionistic natural deduction proof tree for the implicational
-- fragment of propositional logic.
data Proof a
  = -- | An axiom @(Ax c a)@ represents the proof of the sequent @c ⊢ a@, where
    -- the propositional formula @p@ belongs to the context @c@.
    Ax (Context a) (Formula a)
  | -- | Implication elimination (modus ponens). @(ImpElim c b p q)@ represents
    -- the inference that @c ⊢ b@ given a proof @p@ of @c ⊢ a → b@ and a proof
    -- @q@ of @c ⊢ a@.
    ImpElim (Context a) (Formula a) (Proof a) (Proof a)
  | -- | Implication introduction. @(ImpIntr c (Imp a b) p)@ represents the
    -- inference that @c ⊢ a → b@ given a proof @p@ of the sequent @c,a ⊢ b@.
    ImpIntr (Context a) (Formula a) (Proof a)
  deriving (Eq, Show)

-- | Check whether a proof is valid.
checkProof :: Eq a => Proof a -> Bool
checkProof p = case p of
  Ax c a -> a `elem` c
  ImpElim c b p q ->
    conclusion p == Imp (conclusion q) b
      && c `equals` context p
      && c `equals` context q
      && checkProof p
      && checkProof q
  ImpIntr c i p -> case i of
    Imp a b ->
      conclusion p == b
        && (a : c) `equals` context p
        && checkProof p
    _ -> False
  where
    -- Extract the conclusion (without the context) of a proof
    conclusion :: Proof a -> Formula a
    conclusion (Ax _ a) = a
    conclusion (ImpElim _ a _ _) = a
    conclusion (ImpIntr _ a _) = a

    -- Extract the final context of a proof
    context :: Proof a -> Context a
    context (Ax c _) = c
    context (ImpElim c _ _ _) = c
    context (ImpIntr c _ _) = c

    -- Set containment
    subset :: Eq a => Context a -> Context a -> Bool
    subset a b = all (`elem` b) a

    -- Set equality
    equals :: Eq a => Context a -> Context a -> Bool
    equals a b = a `subset` b && b `subset` a

-- | Find an intuitionistic proof of an implication proposition from a context,
-- if such a proof exists.
prove :: Eq a => Context a -> Formula a -> Maybe (Proof a)
prove context i@(Imp a b) = ImpIntr context i <$> prove (a : context) b
prove context v@(Var x) = search context
  where
    -- Scan the context for a formula of the form a1->(a2->(...an->x)...), where
    -- each ai is provable from the context
    search [] = Nothing
    search (f : fs) = case split f of
      Nothing -> search fs
      Just l -> construct (reverse l) v
      where
        construct [] v' = Just $ Ax context v'
        construct (a : as) v' = case prove context a of
          Nothing -> search fs
          Just p -> case construct as (Imp a v') of
            Nothing -> search fs
            Just p' -> Just $ ImpElim context v' p' p

    -- Given a formula of the form a1->(a2->(...an->x)...), extract the list
    -- [a1, a2, ..., an]. For formulas of all other forms, return nothing.
    split (Var y) = if x == y then Just [] else Nothing
    split (Imp a b) = (a :) <$> split b
