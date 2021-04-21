{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AutoProof.Formula
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Formula' type and related functions.
module AutoProof.Formula
  ( -- * Formula type
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- * Formula constructors
    lit,
    true,
    false,
    var,
    not,
    imp,
    or,
    and,
    iff,

    -- * Infix formula constructors
    (-->),
    (\/),
    (/\),
    (<->),

    -- * Pretty-printing
    prettyFormula,

    -- * Operations on formulas
    subformulas,
    substitute,
  )
where

import AutoProof.AST
  ( AST (Root, children, metadata, root),
    ASTMetadata,
    atomicASTConstructor,
    binaryASTConstructor,
    unaryASTConstructor,
  )
import AutoProof.Utils.PrettyPrintable (PrettyPrintable (pretty, prettys))
import AutoProof.Utils.Symbols (andS, falseS, iffS, impS, notS, orS, trueS)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (and, not, or)

-- | Formulas of propositional logic are built inductively from atomic
-- propositions
--
-- * truth \(\top\) ('true'),
-- * falsity \(\bot\) ('false'), and
-- * propositional variables \(a, b, c, \ldots\) ('var')
--
-- using the unary connective
--
-- * negation \(\lnot\) ('not')
--
-- and the binary connectives
--
-- * implication \(\rightarrow\) ('imp', '-->'),
-- * disjunction \(\lor\) ('or', '\/'),
-- * conjunction \(\land\) ('and', '/\'), and
-- * equivalence \(\leftrightarrow\) ('iff', '<->').
data Formula a
  = -- | Propositional literal (truth or falsity). Use 'lit' or 'false' or
    -- 'true'.
    Lit !ASTMetadata Bool
  | -- | Propositional variable. Use 'var'.
    Var !ASTMetadata a
  | -- | Negation. Use 'not'.
    Not !ASTMetadata !(Formula a)
  | -- | Implication. Use 'imp' or '-->'.
    Imp !ASTMetadata !(Formula a) !(Formula a)
  | -- | Disjunction. Use 'or' or '\/'
    Or !ASTMetadata !(Formula a) !(Formula a)
  | -- | Conjunction. Use 'and' or '/\'.
    And !ASTMetadata !(Formula a) !(Formula a)
  | -- | Equivalence. Use 'iff' or '<->'.
    Iff !ASTMetadata !(Formula a) !(Formula a)

instance AST (Formula a) where
  type Root (Formula a) = Maybe a

  root (Var _ x) = Just x
  root _ = Nothing

  children (Lit _ _) = []
  children (Var _ _) = []
  children (Not _ a) = [a]
  children (Imp _ a b) = [a, b]
  children (Or _ a b) = [a, b]
  children (And _ a b) = [a, b]
  children (Iff _ a b) = [a, b]

  metadata (Lit m _) = m
  metadata (Var m _) = m
  metadata (Not m _) = m
  metadata (Imp m _ _) = m
  metadata (Or m _ _) = m
  metadata (And m _ _) = m
  metadata (Iff m _ _) = m

-- Formula constructors

-- | Propositional literal constructor. @('lit' 'True')@ is \(\top\) (i.e.,
-- truth, tautology, or top), and @('lit' 'False')@ is \(\bot\) (i.e., falsity,
-- contradiction, or bottom).
lit :: Bool -> Formula a
lit = atomicASTConstructor Lit

-- | \(\top\) constructor. @'true'@ is an alias for @('lit' 'True')@.
true :: Formula a
true = lit True

-- | \(\bot\) constructor. @'false'@ is an alias for @('lit' 'False')@.
false :: Formula a
false = lit False

-- | Propositional variable constructor. @('var' x)@ represents a propositional
-- variable \(x\).
var :: a -> Formula a
var = atomicASTConstructor Var

-- | Negation constructor. @('not' p)@ represents the formula \(\lnot p\).
--
-- /Note:/ The name of 'not' clashes with 'Prelude.not' from "Prelude".
not :: Formula a -> Formula a
not = unaryASTConstructor Not

-- | Implication constructor. @('imp' p q)@ represents the formula
-- \(p \rightarrow q\).
imp :: Formula a -> Formula a -> Formula a
imp = binaryASTConstructor Imp

-- | Disjunction constructor. @('or' p q)@ represents the formula \(p \lor q\).
--
-- /Note:/ The name of 'or' clashes with 'Prelude.or' from "Prelude".
or :: Formula a -> Formula a -> Formula a
or = binaryASTConstructor Or

-- | Conjunction constructor. @('and' p q)@ represents the formula
-- \(p \land q\).
--
-- /Note:/ The name of 'and' clashes with 'Prelude.and' from "Prelude".
and :: Formula a -> Formula a -> Formula a
and = binaryASTConstructor And

-- | Equivalence constructor. @('iff' p q)@ represents the formula
-- \(p \leftrightarrow q\).
iff :: Formula a -> Formula a -> Formula a
iff = binaryASTConstructor Iff

-- | Right-associative infix alternative for 'imp'. @(p '-->' q)@ represents the
-- implication \(p \rightarrow q\).
--
-- >>> var 'a' --> var 'b' --> var 'c'
-- imp (var 'a') (imp (var 'b') (var 'c'))
(-->) :: Formula a -> Formula a -> Formula a
(-->) = imp

infixr 6 -->

-- | Left-associative infix alternative for 'iff'. @(p '<->' q)@ represents the
-- equivalence \(p \leftrightarrow q\).
--
-- >>> var 'a' <-> var 'b' <-> var 'c'
-- iff (iff (var 'a') (var 'b')) (var 'c')
(<->) :: Formula a -> Formula a -> Formula a
(<->) = iff

infixl 7 <->

-- | Left-associative infix alternative for 'or'. @(p '\/' q)@ represents the
-- disjunction \(p \lor q\).
--
-- >>> var 'a' \/ var 'b' \/ var 'c'
-- or (or (var 'a') (var 'b')) (var 'c')
(\/) :: Formula a -> Formula a -> Formula a
(\/) = or

infixl 8 \/

-- | Left-associative infix alternative for 'and'. @(p '/\' q)@ represents the
-- conjunction \(p \land q\).
--
-- >>> var 'a' /\ var 'b' /\ var 'c'
-- and (and (var 'a') (var 'b')) (var 'c')
(/\) :: Formula a -> Formula a -> Formula a
(/\) = and

infixl 9 /\

-- Instance declarations

instance Show a => Show (Formula a) where
  showsPrec d = f (d > appPrec)
    where
      appPrec = 10

      f _ a@(Lit _ _) = g a
      f b a = showParen b $ g a

      g (Lit _ False) = showString "false"
      g (Lit _ True) = showString "true"
      g (Var _ x) = showString "var " . showsPrec (appPrec + 1) x
      g (Not _ p) = showString "not " . f True p
      g (Imp _ p q) = h "imp " p q
      g (Or _ p q) = h "or " p q
      g (And _ p q) = h "and " p q
      g (Iff _ p q) = h "iff " p q

      h c p q = showString c . f True p . showString " " . f True q

-- | Syntactic equality.
instance Eq a => Eq (Formula a) where
  (Lit _ a) == (Lit _ b) = a == b
  (Var _ a) == (Var _ b) = a == b
  (Not _ a) == (Not _ b) = a == b
  (Imp _ a c) == (Imp _ b d) = a == b && c == d
  (Or _ a c) == (Or _ b d) = a == b && c == d
  (And _ a c) == (And _ b d) = a == b && c == d
  (Iff _ a c) == (Iff _ b d) = a == b && c == d
  _ == _ = False

-- | First compare heights, then compare sizes, then resolve using the
-- convention @Lit < Var < Not < Imp < Or < And < Iff@ (on equality, compare
-- children from left to right).
instance Ord a => Ord (Formula a) where
  compare a b = case compare (metadata a) (metadata b) of
    EQ -> case a of
      Lit _ c -> case b of
        Lit _ d -> compare c d
        _ -> LT
      Var _ c -> case b of
        Lit {} -> GT
        Var _ d -> compare c d
        _ -> LT
      Not _ c -> case b of
        Lit {} -> GT
        Var {} -> GT
        Not _ d -> compare c d
        _ -> LT
      Imp _ c e -> case b of
        Iff {} -> LT
        And {} -> LT
        Or {} -> LT
        Imp _ d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
      Or _ c e -> case b of
        Iff {} -> LT
        And {} -> LT
        Or _ d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
      And _ c e -> case b of
        Iff {} -> LT
        And _ d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
      Iff _ c e -> case b of
        Iff _ d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
    x -> x

instance PrettyPrintable a => PrettyPrintable (Formula a) where
  prettys = f False
    where
      f _ p@(Lit _ _) = g p
      f _ p@(Var _ _) = g p
      f _ p@Not {} = g p
      f b p = showParen b $ g p

      g (Lit _ False) = showString falseS
      g (Lit _ True) = showString trueS
      g (Var _ x) = prettys x
      g (Not _ p) = showString notS . f True p
      g (Imp _ p q) = f True p . padded impS . f True q
      g (Or _ p q) = f True p . padded orS . f True q
      g (And _ p q) = f True p . padded andS . f True q
      g (Iff _ p q) = f True p . padded iffS . f True q

      -- Pad a symbol with one space character on each side
      padded :: String -> ShowS
      padded s = showString (' ' : s) . showString " "

-- | Get a pretty-printed representation of a propositional formula.
--
-- ==== __Examples__
--
-- >>> prettyFormula $ var 'a' --> var 'b' \/ false
-- "a → (b ∨ ⊥)"
prettyFormula :: PrettyPrintable a => Formula a -> String
prettyFormula = pretty

-- Miscellaneuous formula operations

-- | Get the set of subformulas of a propositional formula.
--
-- ==== __Examples__
--
-- >>> subformulas $ or (var 'x') (and (var 'y') (var 'z'))
-- fromList [var 'x',var 'y',var 'z',and (var 'y') (var 'z'),or (var 'x') (and (var 'y') (var 'z'))]
subformulas :: Ord a => Formula a -> Set (Formula a)
subformulas = go Set.empty
  where
    go s p@(Lit _ _) = Set.insert p s
    go s p@(Var _ _) = Set.insert p s
    go s p@(Not _ a) = go (Set.insert p s) a
    go s p@(Imp _ a b) = go (go (Set.insert p s) a) b
    go s p@(Or _ a b) = go (go (Set.insert p s) a) b
    go s p@(And _ a b) = go (go (Set.insert p s) a) b
    go s p@(Iff _ a b) = go (go (Set.insert p s) a) b

-- | @('substitute' a x p)@ represents \(a[x := p]\), the substitution of each
-- occurence of the variable \(x\) in the formula \(a\) by the formula \(p\).
--
-- ==== __Examples__
--
-- >>> substitute (var 'e' --> var 'e') 'e' (var 'a' /\ var 'a')
-- imp (and (var 'a') (var 'a')) (and (var 'a') (var 'a'))
substitute :: Eq a => Formula a -> a -> Formula a -> Formula a
substitute a@(Lit _ _) _ _ = a
substitute v@(Var _ y) x p = if x == y then p else v
substitute (Not _ a) x p = not $ substitute a x p
substitute (Imp _ a b) x p = imp (substitute a x p) (substitute b x p)
substitute (Or _ a b) x p = or (substitute a x p) (substitute b x p)
substitute (And _ a b) x p = and (substitute a x p) (substitute b x p)
substitute (Iff _ a b) x p = iff (substitute a x p) (substitute b x p)
