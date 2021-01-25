{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Data.Prop.Types
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definition of types for representing propositional logic formulas.
module Data.Prop.Types
  ( -- * Types
    Formula (Lit, Var, Not, Imp, Or, And),
    Context,
    Sequent,

    -- * Constructors
    true,
    false,
    lit,
    var,
    not,
    imp,
    implies,
    or,
    and,
    iff,
    (-->),
    (\/),
    (/\),
    (<->),

    -- * Operations on formulas
    subformulas,
    substitute,

    -- * Pretty-printing
    prettyFormula,
    prettyContext,
    prettySequent,
  )
where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Prop.Utils (PrettyPrintable (pretty))
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (and, not, or)

-- | Formulas of propositional logic are built inductively from atomic
-- propositions
--
-- * truth \(\top\),
-- * falsity \(\bot\), and
-- * propositional variables \(a, b, c, \ldots\)
--
-- using the unary connective
--
-- * negation \(\lnot\)
--
-- and the binary connectives
--
-- * implication \(\rightarrow\),
-- * disjunction \(\lor\), and
-- * conjunction \(\land\).
data Formula a
  = -- | Top or bottom proposition literal. @('Lit' 'True')@ is \(\top\) (i.e.,
    -- truth, tautology, or top) and @('Lit' 'False')@ is \(\bot\) (i.e.,
    -- falsity, contradiction, or bottom).
    Lit Bool
  | -- | Propositional variable. @('Var' x)@ represents a variable named \(x\).
    Var a
  | -- | Negation. @('Not' p)@ represents the formula \(\lnot p\).
    Not (Formula a)
  | -- | Implication. @('Imp' p q)@ represents the formula \(p \rightarrow q\).
    Imp (Formula a) (Formula a)
  | -- | Disjunction. @('Or' p q)@ represents the formula \(p \lor q\).
    Or (Formula a) (Formula a)
  | -- | Conjunction. @('And' p q)@ represents the formula \(p \land q\).
    And (Formula a) (Formula a)
  deriving (Eq, Ord, Show)

-- | A set of propositional formulas, used as antecedents of a sequent.
type Context a = Set (Formula a)

-- | A pair @(c, p)@ represents the sequent or judgement \(c \vdash p\).
type Sequent a = (Context a, Formula a)

-- Pretty-printing

-- | @('prettyFormula' a)@ is a human-readable representation of the
-- propositional formula \(a\).
--
-- ==== __Examples__
--
-- >>> prettyFormula $ And (Or (Var "x") (Var "y")) (not $ Var "z")
-- "(x | y) & (~z)"

-- >>> prettyFormula $ not $ And (Lit True --> Var 'x') (Var 'z')
-- "~((true -> x) & z)"
prettyFormula :: PrettyPrintable a => Formula a -> String
prettyFormula = f False
  where
    -- Apply @g@ and optionally wrap the result in parentheses. Variables
    -- never get parentheses.
    f _ v@(Lit _) = g v
    f _ v@(Var _) = g v
    f parentheses t = if parentheses then "(" ++ g t ++ ")" else g t

    -- Recursive pretty-printer
    g (Lit True) = "true"
    g (Lit False) = "false"
    g (Var x) = pretty x
    g (Not p) = '~' : f True p
    g (Imp p q) = f True p ++ " -> " ++ f True q
    g (And p q) = f True p ++ " & " ++ f True q
    g (Or p q) = f True p ++ " | " ++ f True q

-- | @('prettyContext' c)@ is a human-readable representation of a context
-- \(c\).
--
-- ==== __Examples__
--
-- >>> prettyContext [Var "x", Var "y", Imp (Var "x") (Var "y")]
-- "x, y, x -> y"
prettyContext :: (PrettyPrintable a, Foldable f) => f (Formula a) -> String
prettyContext c = intercalate ", " (pretty <$> toList c)

-- | @('prettySequent' c a)@ is a human-readable representation of a sequent
-- \(c \vdash a\).
--
-- ==== __Examples__
--
-- >>> prettySequent [Var "x", Imp (Var "x") (Var "y")] (Var "y")
-- "x, x -> y |- y"
prettySequent :: (PrettyPrintable a, Foldable f) => f (Formula a) -> Formula a -> String
prettySequent c a = case prettyContext c of
  "" -> "|- " ++ pretty a
  c' -> c' ++ " |- " ++ pretty a

instance PrettyPrintable a => PrettyPrintable (Formula a) where
  pretty = prettyFormula

instance PrettyPrintable a => PrettyPrintable (Context a) where
  pretty = prettyContext

instance PrettyPrintable a => PrettyPrintable (Sequent a) where
  pretty = uncurry prettySequent

-- Additional constructors

-- | 'true' is the propositional constant \(\top\) (i.e., truth, tautology, or
-- top).
true :: Formula a
true = Lit True

-- | 'false' is the propositional constant \(\bot\) (i.e., falsity,
-- contradiction, or bottom).
false :: Formula a
false = Lit False

-- | @('lit' 'True')@ is \(\top\), and @('lit' 'False')@ is \(\bot\).
lit :: Bool -> Formula a
lit True = true
lit False = false

-- | @('var' x)@ represents a propositional variable \(x\).
var :: a -> Formula a
var = Var

-- | @('not' p)@ represents the negation \(\lnot p\).
not :: Formula a -> Formula a
not = Not

-- | @('imp' p q)@ represents the implication \(p \rightarrow q\).
imp :: Formula a -> Formula a -> Formula a
imp = Imp

-- | @('implies' p q)@ represents the implication \(p \rightarrow q\). This is
-- an alias for 'imp'.
implies :: Formula a -> Formula a -> Formula a
implies = imp

-- | @('or' p q)@ represents the disjunction \(p \lor q\).
or :: Formula a -> Formula a -> Formula a
or = Or

-- | @('and' p q)@ represents the conjunction \(p \rightarrow q\).
and :: Formula a -> Formula a -> Formula a
and = And

-- | @('iff' p q)@ represents the equivalence \(p \leftrightarrow q\), which
-- abbreviates \((p \rightarrow q) \wedge (q \rightarrow p)\).
iff :: Formula a -> Formula a -> Formula a
iff p q = (p --> q) /\ (q --> p)

-- | Right-associative infix alternative for 'Imp'. @(p '-->' q)@ represents the
-- implication \(p \rightarrow q\).
--
-- >>> Var 'a' --> Var 'b' --> Var 'c'
-- Imp (Var 'a') (Imp (Var 'b') (Var 'c'))
(-->) :: Formula a -> Formula a -> Formula a
(-->) = Imp

infixr 5 -->

-- | Left-associative infix alternative for 'Or'. @(p '\/' q)@ represents the
-- disjunction \(p \lor q\).
--
-- >>> Var 'a' \/ Var 'b' \/ Var 'c'
-- Or (Or (Var 'a') (Var 'b')) (Var 'c')
(\/) :: Formula a -> Formula a -> Formula a
(\/) = Or

infixl 6 \/

-- | Left-associative infix alternative for 'And'. @(p '/\' q)@ represents the
-- conjunction \(p \land q\).
--
-- >>> Var 'a' /\ Var 'b' /\ Var 'c'
-- And (And (Var 'a') (Var 'b')) (Var 'c')
(/\) :: Formula a -> Formula a -> Formula a
(/\) = And

infixl 6 /\

-- | Left-associative infix alternative for 'iff'. @(p '<->' q)@ represents the
-- equivalence \(p \leftrightarrow q\).
(<->) :: Formula a -> Formula a -> Formula a
(<->) = iff

infixr 5 <->

-- Miscellaneuous
-- TODO: put functions below somewhere else.

-- | Return the set of subformulas of a propositional formula.
--
-- ==== __Examples__
--
-- >>> subformulas $ imp (var 'x') (var 'y')
-- fromList [Var 'x',Var 'y',Imp (Var 'x') (Var 'y')]
subformulas :: Ord a => Formula a -> Set (Formula a)
subformulas = go Set.empty
  where
    go s p@(Lit _) = Set.insert p s
    go s p@(Var _) = Set.insert p s
    go s p@(Not a) = go (Set.insert p s) a
    go s p@(Imp a b) = go (go (Set.insert p s) a) b
    go s p@(Or a b) = go (go (Set.insert p s) a) b
    go s p@(And a b) = go (go (Set.insert p s) a) b

-- | @('substitute' a x p)@ represents \(a[x := p]\), the substitution of each
-- occurence of the variable \(x\) in the formula \(a\) by the formula \(p\).
--
-- ==== __Examples__
--
-- >>> substitute (var 'e' --> var 'e') 'e' (var 'a' /\ var 'a')
-- Imp (And (Var 'a') (Var 'a')) (And (Var 'a') (Var 'a'))
substitute :: Eq a => Formula a -> a -> Formula a -> Formula a
substitute a@(Lit _) _ _ = a
substitute v@(Var y) x p = if x == y then p else v
substitute (Not a) x p = not $ substitute a x p
substitute (Imp a b) x p = imp (substitute a x p) (substitute b x p)
substitute (Or a b) x p = or (substitute a x p) (substitute b x p)
substitute (And a b) x p = and (substitute a x p) (substitute b x p)
