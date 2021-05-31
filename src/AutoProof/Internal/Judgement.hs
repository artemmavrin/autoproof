-- |
-- Module      : AutoProof.Internal.Judgement
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Judgement' type and related functions.
module AutoProof.Internal.Judgement
  ( -- * Types and constructors
    Context,
    Judgement (Judgement, antecedents, succedent),
    (|-),

    -- * Pretty-printing
    prettyJudgement,

    -- * Operations on judgements
    weakenJudgement,
  )
where

import AutoProof.Internal.Formula (Formula)
import AutoProof.Internal.Utils.PrettyPrintable
  ( PrettyPrintable (pretty),
    prettySeq,
  )
import AutoProof.Internal.Utils.Set (toSet)
import AutoProof.Internal.Utils.Symbols (turnstileS)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A set of propositional formulas, used as antecedents of a judgement.
type Context a = Set (Formula a)

-- | @(Judgement c a)@ represents the judgement or sequent \(c \vdash a\).
--
-- >>> Judgement Set.empty (Imp (And (Var 'a') (Var 'b')) (Var 'a'))
-- [] |- Imp (And (Var 'a') (Var 'b')) (Var 'a')
data Judgement a = Judgement
  { -- | The antecedents (or hypotheses).
    antecedents :: Context a,
    -- | The succedent (or consequent, or conclusion).
    succedent :: Formula a
  }
  deriving (Eq)

-- | Infix judgement constructor. @(c '|-' a)@ represents the judgement
-- \(c \vdash a\).
--
-- >>> [Var 'a', Imp (Var 'a') (Var 'b')] |- Var 'b'
-- [Var 'a',Imp (Var 'a') (Var 'b')] |- Var 'b'
--
-- /Note:/ If @c@ is already a 'Data.Set.Set', then it is recommended to use
-- @(Judgement c a)@ in favor of @(c '|-' a)@, since the latter will create a
-- new set and fill it with the values in @c@.
(|-) :: (Ord a, Foldable f) => f (Formula a) -> Formula a -> Judgement a
c |- a = Judgement (toSet c) a

infix 5 |-

-- Compare judgements based on the consequent first
instance Ord a => Ord (Judgement a) where
  compare (Judgement g a) (Judgement g' a') = case compare a a' of
    EQ -> compare g g'
    x -> x

instance Show a => Show (Judgement a) where
  showsPrec d (Judgement c a) =
    showParen (d > turnstilePrec) $
      showsPrec (turnstilePrec + 1) (Set.toList c)
        . showString " |- "
        . showsPrec (turnstilePrec + 1) a
    where
      turnstilePrec = 5

instance (Ord a, Read a) => Read (Judgement a) where
  readsPrec d = readParen (d > turnstilePrec) readJudgement
    where
      turnstilePrec = 5
      readJudgement s =
        [ (g |- a, v)
          | (g, t) <- readList s,
            ("|-", u) <- lex t,
            (a, v) <- readsPrec (turnstilePrec + 1) u
        ]

instance PrettyPrintable a => PrettyPrintable (Judgement a) where
  pretty (Judgement c a) = case prettySeq c of
    "" -> turnstileS ++ " " ++ pretty a
    c' -> c' ++ (' ' : turnstileS) ++ " " ++ pretty a

-- | Get a pretty-printed representation of a judgement.
--
-- ==== __Examples__
--
-- >>> prettyJudgement $ [Var 'a', Imp (Var 'a') (Var 'b')] |- Var 'b'
-- "a, a → b ⊢ b"
prettyJudgement :: PrettyPrintable a => Judgement a -> String
prettyJudgement = pretty

-- | Weaken a judgement by inserting a formula into its hypotheses.
--
-- >>> weakenJudgement ([Var 'a', Imp (Var 'a') (Var 'b')] |- Var 'b') (Var 'c')
-- [Var 'a',Var 'c',Imp (Var 'a') (Var 'b')] |- Var 'b'
weakenJudgement :: Ord a => Judgement a -> Formula a -> Judgement a
weakenJudgement (Judgement c a) b = Judgement (Set.insert b c) a
