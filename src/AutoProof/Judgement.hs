-- |
-- Module      : AutoProof.Judgement
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Judgement' type and related functions.
module AutoProof.Judgement
  ( -- * Types and constructors
    Context,
    Judgement (Judgement, antecedents, consequent),
    (|-),

    -- * Pretty-printing
    prettyJudgement,

    -- * Operations on judgements
    weakenJudgement,
  )
where

import AutoProof.Formula (Formula)
import AutoProof.Utils.PrettyPrintable
  ( PrettyPrintable (pretty),
    prettySeq,
  )
import AutoProof.Utils.Set (toSet)
import AutoProof.Utils.Symbols (turnstileS)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A set of propositional formulas, used as antecedents of a judgement.
type Context a = Set (Formula a)

-- | @(Judgement c a)@ represents the judgement \(c \vdash a\).
--
-- >>> Judgement Set.empty (imp (and (var 'a') (var 'b')) (var 'a'))
-- [] |- imp (and (var 'a') (var 'b')) (var 'a')
data Judgement a = Judgement
  { -- | The antecedents or hypotheses.
    antecedents :: Context a,
    -- | The consequent or conclusion.
    consequent :: Formula a
  }
  deriving (Eq, Ord)

-- | Infix judgement constructor. @(c '|-' a)@ represents the judgement
-- \(c \vdash a\).
--
-- >>> [var 'a', var 'a' --> var 'b'] |- var 'b'
-- [var 'a',imp (var 'a') (var 'b')] |- var 'b'
--
-- /Note:/ If @c@ is already a 'Data.Set.Set', then it is recommended to use
-- @(Judgement c a)@ in favor of @(c '|-' a)@, since the latter will create a
-- new set and fill it with the values in @c@.
(|-) :: (Ord a, Foldable f) => f (Formula a) -> Formula a -> Judgement a
c |- a = Judgement (toSet c) a

infix 5 |-

instance Show a => Show (Judgement a) where
  showsPrec d (Judgement c a) =
    showParen (d > turnstilePrec) $
      showsPrec (turnstilePrec + 1) (Set.toList c)
        . showString " |- "
        . showsPrec (turnstilePrec + 1) a
    where
      turnstilePrec = 5

instance PrettyPrintable a => PrettyPrintable (Judgement a) where
  pretty (Judgement c a) = case prettySeq c of
    "" -> turnstileS ++ " " ++ pretty a
    c' -> c' ++ (' ' : turnstileS) ++ " " ++ pretty a

-- | Get a pretty-printed representation of a judgement.
--
-- >>> prettyJudgement $ [var 'a', var 'a' --> var 'b'] |- var 'b'
-- "a, a → b ⊢ b"
prettyJudgement :: PrettyPrintable a => Judgement a -> String
prettyJudgement = pretty

-- | Weaken a judgement by inserting a formula into its hypotheses.
--
-- >>> weakenJudgement ([var 'a', var 'a' --> var 'b'] |- var 'b') (var 'c')
-- [var 'a',var 'c',imp (var 'a') (var 'b')] |- var 'b'
weakenJudgement :: Ord a => Judgement a -> Formula a -> Judgement a
weakenJudgement (Judgement c a) b = Judgement (Set.insert b c) a
