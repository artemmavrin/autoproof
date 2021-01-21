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
  ( -- * Propositional formula type
    Formula (Lit, Var, Not, Imp, Or, And),

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
  )
where

import Prelude hiding (and, not, or)
import Data.Prop.Internal.Utils

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

-- | @('pretty' p)@ is a human-readable representation of a formula \(p\).
--
-- ==== __Examples__
--
-- >>> pretty $ And (Or (Var "x") (Var "y")) (not $ Var "z")
-- "(x | y) & (z -> false)"
--
-- >>> pretty $ not $ And (Lit True --> Var 'x') (Var 'z')
-- "((true -> x) & z) -> false"
instance PrettyPrintable a => PrettyPrintable (Formula a) where
  pretty = f False
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
      g (Not p) = '-' : f True p
      g (Imp p q) = f True p ++ " -> " ++ f True q
      g (And p q) = f True p ++ " & " ++ f True q
      g (Or p q) = f True p ++ " | " ++ f True q

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
