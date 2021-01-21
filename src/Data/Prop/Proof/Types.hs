-- |
-- Module      : Data.Prop.Proof.Types
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Natural deduction proof trees for intuitionistic propositional logic.
module Data.Prop.Proof.Types
  ( -- * Types
    Proof
      ( Ax,
        TopIntr,
        BotElim,
        NotElim,
        NotIntr,
        ImpElim,
        ImpIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        AndElimL,
        AndElimR,
        AndIntr
      ),
    Context,

    -- * Debugging
    debug,
    valid,
  )
where

import Data.Either (isRight)
import Data.Prop.Types (Formula (And, Imp, Lit, Not, Or))
import Data.Set (Set)
import qualified Data.Set as Set

-- | A set of propositional formulas, used as antecedents of a sequent.
type Context a = Set (Formula a)

-- | A natural deduction proof tree for intuitionistic propositional logic.
data Proof a
  = -- | An axiom @('Ax' g a)@ represents the inference of the sequent
    -- \(g \vdash a\), where the propositional formula \(a\) belongs to the
    -- context \(g\).
    Ax (Context a) (Formula a)
  | -- | Truth introduction. @('TopIntr' g ('Lit' 'True'))@ represents an
    -- inference of the sequent \(g \vdash \top\).
    TopIntr (Context a) (Formula a)
  | -- | Falsity elimination (principle of explosion). @('BotElim' g a p)@
    -- represents the inference \(g \vdash a\) from a proof \(p\) of
    -- \(p \vdash \bot\).
    BotElim (Context a) (Formula a) (Proof a)
  | -- | Negation elimination. @('NotElim' g ('Lit' 'False') p q)@ represents
    -- the inference of the sequent \(g \vdash \bot\) given a proof \(p\) of
    -- \(g \vdash \lnot a\) and a proof \(q\) of \(g \vdash a\).
    NotElim (Context a) (Formula a) (Proof a) (Proof a)
  | -- | Negation introduction. @('NotIntr' g ('Not' a) p)@ represents the
    -- inference of the sequent \(g \vdash \neg a\) from a proof \(p\) of
    -- \(g, a \vdash \bot\).
    NotIntr (Context a) (Formula a) (Proof a)
  | -- | Implication elimination (modus ponens). @('ImpElim' g b p q)@
    -- represents the inference of the sequent \(g \vdash b\) given a proof
    -- \(p\) of \(g \vdash a \rightarrow b\) and a proof \(q\) of
    -- \(g \vdash q\).
    ImpElim (Context a) (Formula a) (Proof a) (Proof a)
  | -- | Implication introduction. @('ImpIntr' g ('Imp' a b) p)@ represents the
    -- inference of the sequent \(g \vdash a \rightarrow b\) given a proof \(p\)
    -- of \(g, a \vdash b\).
    ImpIntr (Context a) (Formula a) (Proof a)
  | -- | Disjunction elimination. @('OrElim' g c p q r)@ represents an inference
    -- of the sequent \(g \vdash c\) given a proof \(p\) of
    -- \(g \vdash a \lor b\), a proof \(q\) of \(g, a \vdash c\), and a proof
    -- \(r\) of \(g, b \vdash c\).
    OrElim (Context a) (Formula a) (Proof a) (Proof a) (Proof a)
  | -- | Disjunction introduction (left). @('OrIntrL' g ('Or' a b) p)@
    -- represents an inference of the sequent \(g \vdash a \lor b\) given a
    -- proof \(p\) of \(g \vdash a\).
    OrIntrL (Context a) (Formula a) (Proof a)
  | -- | Disjunction introduction (right). @('OrIntrR' g ('Or' a b) p)@
    -- represents an inference of the sequent \(g \vdash a \lor b\) given a
    -- proof \(p\) of \(g \vdash b\).
    OrIntrR (Context a) (Formula a) (Proof a)
  | -- | Conjunction elimination (left). @('AndElimL` g a p)@ represents the
    -- inference of the sequent \(g \vdash a\) given a proof \(p\) of
    -- \(g \vdash a \land b\).
    AndElimL (Context a) (Formula a) (Proof a)
  | -- | Conjunction elimination (right). @('AndElimR` g b p)@ represents the
    -- inference of the sequent \(g \vdash b\) given a proof \(p\) of
    -- \(g \vdash a \land b\).
    AndElimR (Context a) (Formula a) (Proof a)
  | -- | Conjunction introduction. @('AndIntr' g ('And' a b) p q)@ represents
    -- the inference of the sequent \(g \vdash a \land b\) given a proof \(p\)
    -- of \(g \vdash a\) and a proof \(q\) of \(g \vdash b\).
    AndIntr (Context a) (Formula a) (Proof a) (Proof a)
  deriving (Eq, Show)

-- | Return an invalid inference node (on the 'Left'), if there is one.
-- Otherwise, return @'Right' ()@.
debug :: Ord a => Proof a -> Either (Proof a) ()
debug proof = case proof of
  Ax g a -> if Set.member a g then return () else Left proof
  TopIntr _ (Lit True) -> return ()
  BotElim g _ p -> case conclusion p of
    Lit False -> if context p == g then debug p else Left proof
    _ -> Left proof
  NotElim g (Lit False) p q -> case conclusion p of
    Not a ->
      if conclusion q == a && context p == g && context q == g
        then do
          debug p
          debug q
        else Left proof
    _ -> Left proof
  NotIntr g (Not a) p -> case conclusion p of
    Lit False -> if context p == Set.insert a g then debug p else Left proof
    _ -> Left proof
  ImpElim g b p q ->
    if conclusion p == Imp (conclusion q) b
      && g == context p
      && g == context q
      then do
        debug p
        debug q
      else Left proof
  ImpIntr g (Imp a b) p ->
    if conclusion p == b && Set.insert a g == context p
      then debug p
      else Left proof
  AndElimL g a p ->
    case conclusion p of
      And a' _ ->
        if a == a' && context p == g
          then debug p
          else Left proof
      _ -> Left proof
  AndElimR g b p ->
    case conclusion p of
      And _ b' ->
        if b == b' && context p == g
          then debug p
          else Left proof
      _ -> Left proof
  AndIntr g (And a b) p q ->
    if conclusion p == a
      && conclusion q == b
      && context p == g
      && context q == g
      then do
        debug p
        debug q
      else Left proof
  OrElim g c p q r ->
    case conclusion p of
      Or a b ->
        if conclusion q == c
          && conclusion r == c
          && context p == g
          && context q == Set.insert a g
          && context r == Set.insert b g
          then do
            debug p
            debug q
            debug r
          else Left proof
      _ -> Left proof
  OrIntrL g (Or a _) p ->
    if conclusion p == a && context p == g
      then debug p
      else Left proof
  OrIntrR g (Or _ b) p ->
    if conclusion p == b && context p == g
      then debug p
      else Left proof
  _ -> Left proof -- Pattern match failures from introduction rules
  where
    -- Extract the conclusion (without the context) of a proof
    conclusion :: Proof a -> Formula a
    conclusion (Ax _ a) = a
    conclusion (TopIntr _ a) = a
    conclusion (BotElim _ a _) = a
    conclusion (NotElim _ a _ _) = a
    conclusion (NotIntr _ a _) = a
    conclusion (ImpElim _ a _ _) = a
    conclusion (ImpIntr _ a _) = a
    conclusion (OrElim _ a _ _ _) = a
    conclusion (OrIntrL _ a _) = a
    conclusion (OrIntrR _ a _) = a
    conclusion (AndElimL _ a _) = a
    conclusion (AndElimR _ a _) = a
    conclusion (AndIntr _ a _ _) = a

    -- Extract the final context of a proof
    context :: Proof a -> Context a
    context (Ax c _) = c
    context (TopIntr c _) = c
    context (BotElim c _ _) = c
    context (NotElim c _ _ _) = c
    context (NotIntr c _ _) = c
    context (ImpElim c _ _ _) = c
    context (ImpIntr c _ _) = c
    context (OrElim c _ _ _ _) = c
    context (OrIntrL c _ _) = c
    context (OrIntrR c _ _) = c
    context (AndElimL c _ _) = c
    context (AndElimR c _ _) = c
    context (AndIntr c _ _ _) = c

-- | Check whether a proof is valid.
valid :: Ord a => Proof a -> Bool
valid p = isRight (debug p)
