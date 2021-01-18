-- |
-- Module      : Proof.Implication
-- Copyright   : (c) Artem Mavrin, 2020
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intuitionistic proofs in the implicational fragment of propositional logic.
module Proof.Implication where

-- | Formulas in the implicational fragment of propositional logic
data Formula a
  = -- | @(Var x)@ represents a propositional variable /x/.
    Var a
  | -- | @(p :-> q)@ represents the implication /p → q/ between formulas /p/ and
    -- /q/.
    (Formula a) :-> (Formula a)
  deriving (Eq, Ord, Show)

-- | A sequence of propositional formulas.
type Context a = [Formula a]

-- | @(c :|- p)@ represents the sequent (or judgement) /c ⊢ p/, which should be
-- interpreted as "if the propositions in /c/ hold, then so does /p/".
data Sequent a = (Context a) :|- (Formula a)
  deriving (Eq, Show)

-- | A natural deduction proof tree for the implicational fragment.
data Proof a
  = -- | An axiom is a sequent @(c :|- p)@ where @p@ is an element of @c@.
    Ax (Sequent a)
  | -- | Implication elimination (modus ponens)
    ImpElim (Sequent a) (Proof a) (Proof a)
  | -- | Implication introduction
    ImpIntr (Sequent a) (Proof a)
  deriving (Eq, Show)

infixr 1 :->

infix 2 :|-

-- | Find an intuitionistic proof of an implication proposition from a context,
-- if such a proof exists.
prove :: Eq a => Context a -> Formula a -> Maybe (Proof a)
prove context i@(a :-> b) = ImpIntr (context :|- i) <$> prove (a : context) b
prove context v@(Var x) = prove' context
  where
    -- TODO: better name for prove'
    prove' [] = Nothing
    prove' (f : fs) = case split x f of
      Nothing -> prove' fs
      Just l -> g (reverse l) v
      where
        -- TODO: better name for g
        g [] v' = Just $ Ax (context :|- v')
        g (a : as) v' = case prove context a of
          Nothing -> prove' fs
          Just p -> case g as (a :-> v') of
            Nothing -> prove' fs
            Just p' -> Just $ ImpElim (context :|- v') p' p

    -- Given a variable x and a formula a1 -> (a2-> (...-> (an -> x)...)),
    -- extract the list [a1, a2, ..., an]. For formulas of all other forms,
    -- return nothing.
    split :: Eq a => a -> Formula a -> Maybe [Formula a]
    split x (Var y) = if x == y then Just [] else Nothing
    split x (a :-> b) = (a :) <$> split x b
