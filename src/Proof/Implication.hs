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
    Proof (Ax, ImpIntr, ImpElim),

    -- * Constructors
    (-->),

    -- * Proof search
    prove,

    -- * Debugging
    debug,
    valid,
  )
where

import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Formulas in the implicational fragment of propositional logic
data Formula a
  = -- | @(Var x)@ represents a propositional variable @x@.
    Var a
  | -- | @(Imp a b)@ represents the implication @a → b@ between propositional
    -- formulas @a@ and @b@.
    Imp (Formula a) (Formula a)
  deriving (Eq, Ord, Show)

-- | Right-associative infix alternative for 'Imp'.
(-->) :: Formula a -> Formula a -> Formula a
(-->) = Imp

infixr 1 -->

-- | A set of propositional formulas.
type Context a = Set (Formula a)

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

-- | Return an invalid inference node, if there is one.
debug :: Ord a => Proof a -> Maybe (Proof a)
debug proof = case proof of
  Ax c a -> if Set.member a c then Nothing else Just proof
  ImpElim c b p q ->
    if conclusion p == Imp (conclusion q) b
      && c == context p
      && c == context q
      then case debug p of
        Nothing -> debug q
        p' -> p'
      else Just proof
  ImpIntr c i p ->
    case i of
      Imp a b ->
        if conclusion p == b && (Set.insert a c == context p)
          then debug p
          else Just proof
      _ -> Just proof
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

-- | Check whether a proof is valid.
valid :: Ord a => Proof a -> Bool
valid p = isNothing (debug p)

-- | @(prove c a)@ finds an intuitionistic proof of a sequent @c ⊢ a@, if such a
-- proof exists.
prove :: (Ord a, Foldable t) => t (Formula a) -> Formula a -> Maybe (Proof a)
prove context = prove' (foldr Set.insert Set.empty context)
  where
    prove' :: Ord a => Context a -> Formula a -> Maybe (Proof a)
    prove' c i@(Imp a b) = ImpIntr c i <$> prove' (Set.insert a c) b
    prove' c v@(Var x) = foldl search Nothing c
      where
        -- Scan the context for a formula of the form a1->(a2->(...an->x)...),
        -- where each ai is provable from the context
        search (Just p) _ = Just p
        search _ f = case split f of
          Nothing -> Nothing
          Just l -> construct (reverse l) v
          where
            construct [] v' = Just $ Ax c v'
            construct (a : as) v' = case prove' c a of
              Nothing -> Nothing
              Just p -> case construct as (Imp a v') of
                Nothing -> Nothing
                Just p' -> Just $ ImpElim c v' p' p

            -- Given a formula of the form a1->(a2->(...an->x)...), extract the list
            -- [a1, a2, ..., an]. For formulas of all other forms, return nothing.
            split (Var y) = if x == y then Just [] else Nothing
            split (Imp a b) = (a :) <$> split b
