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
    Height,
    Size,

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

    -- * Formula metadata
    height,
    size,
  )
where

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
-- * implication \(\rightarrow\) ('imp'),
-- * disjunction \(\lor\) ('or'),
-- * conjunction \(\land\) ('and'), and
-- * equivalence \(\leftrightarrow\) ('iff').
data Formula a
  = -- | Propositional literal (truth or falsity).
    Lit !Bool
  | -- | Propositional variable.
    Var !a
  | -- | Negation.
    Not !Height !Size !(Formula a)
  | -- | Implication.
    Imp !Height !Size !(Formula a) !(Formula a)
  | -- | Disjunction.
    Or !Height !Size !(Formula a) !(Formula a)
  | -- | Conjunction.
    And !Height !Size !(Formula a) !(Formula a)
  | -- | Equivalence.
    Iff !Height !Size !(Formula a) !(Formula a)

-- | Height of a formula
type Height = Int

-- | Size of a formula
type Size = Int

-- | \(O(1)\). Get the height of a propositional formula.
--
-- The /height/ of a propositional formula is its height as a rooted tree (i.e.,
-- the number of edges on the longest path from the root to a leaf).
--
-- >>> height $ imp (var 'a') (or (var 'b') (var 'c'))
-- 2
height :: Formula a -> Int
height (Lit _) = 0
height (Var _) = 0
height (Not h _ _) = h
height (Imp h _ _ _) = h
height (Or h _ _ _) = h
height (And h _ _ _) = h
height (Iff h _ _ _) = h

-- | \(O(1)\). Get the size of a propositional formula.
--
-- The /size/ of a propositional formula is the number of symbols (atomic
-- propositions and connectives) that make up that formula.
--
-- >>> size $ imp (var 'a') (or (var 'b') (var 'c'))
-- 5
size :: Formula a -> Int
size (Lit _) = 1
size (Var _) = 1
size (Not _ s _) = s
size (Imp _ s _ _) = s
size (Or _ s _ _) = s
size (And _ s _ _) = s
size (Iff _ s _ _) = s

-- | Propositional literal constructor. @('lit' 'True')@ is \(\top\) (i.e.,
-- truth, tautology, or top), and @('lit' 'False')@ is \(\bot\) (i.e., falsity,
-- contradiction, or bottom).
lit :: Bool -> Formula a
lit = Lit

-- | \(\top\) constructor. @'true'@ is an alias for @('lit' 'True')@.
true :: Formula a
true = lit True

-- | \(\bot\) constructor. @'false'@ is an alias for @('lit' 'False')@.
false :: Formula a
false = lit False

-- | Propositional variable constructor. @('var' x)@ represents a propositional
-- variable \(x\).
var :: a -> Formula a
var = Var

-- | Negation constructor. @('not' p)@ represents the formula \(\lnot p\).
--
-- /Note:/ The name of 'not' clashes with 'Prelude.not' from "Prelude".
not :: Formula a -> Formula a
not p = Not (1 + height p) (1 + size p) p

-- | Implication constructor. @('imp' p q)@ represents the formula
-- \(p \rightarrow q\).
imp :: Formula a -> Formula a -> Formula a
imp = binaryConstructor Imp

-- | Disjunction constructor. @('or' p q)@ represents the formula \(p \lor q\).
--
-- /Note:/ The name of 'or' clashes with 'Prelude.or' from "Prelude".
or :: Formula a -> Formula a -> Formula a
or = binaryConstructor Or

-- | Conjunction constructor. @('and' p q)@ represents the formula
-- \(p \land q\).
--
-- /Note:/ The name of 'and' clashes with 'Prelude.and' from "Prelude".
and :: Formula a -> Formula a -> Formula a
and = binaryConstructor And

-- | Equivalence constructor. @('iff' p q)@ represents the formula
-- \(p \leftrightarrow q\).
iff :: Formula a -> Formula a -> Formula a
iff = binaryConstructor Iff

-- Helper function for formula constructors that handles height and size
-- calculation
binaryConstructor ::
  (Int -> Int -> Formula a -> Formula a -> Formula a) ->
  Formula a ->
  Formula a ->
  Formula a
binaryConstructor c p q = c d s p q
  where
    d = 1 + max (height p) (height q)
    s = 1 + size p + size q

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

      f _ a@(Lit _) = g a
      f b a = showParen b $ g a

      g (Lit False) = showString "false"
      g (Lit True) = showString "true"
      g (Var x) = showString "var " . showsPrec (appPrec + 1) x
      g (Not _ _ p) = showString "not " . f True p
      g (Imp _ _ p q) = h "imp " p q
      g (Or _ _ p q) = h "or " p q
      g (And _ _ p q) = h "and " p q
      g (Iff _ _ p q) = h "iff " p q

      h c p q = showString c . f True p . showString " " . f True q

instance Eq a => Eq (Formula a) where
  (Lit a) == (Lit b) = a == b
  (Var a) == (Var b) = a == b
  (Not _ _ a) == (Not _ _ b) = a == b
  (Imp _ _ a c) == (Imp _ _ b d) = a == b && c == d
  (Or _ _ a c) == (Or _ _ b d) = a == b && c == d
  (And _ _ a c) == (And _ _ b d) = a == b && c == d
  (Iff _ _ a c) == (Iff _ _ b d) = a == b && c == d
  _ == _ = False

instance Ord a => Ord (Formula a) where
  compare a b = case compare (height a) (height b) of
    EQ -> case compare (size a) (size b) of
      EQ -> case a of
        Lit c -> case b of
          Lit d -> compare c d
          _ -> LT
        Var c -> case b of
          Lit _ -> GT
          Var d -> compare c d
          _ -> LT
        Not _ _ c -> case b of
          Lit _ -> GT
          Var _ -> GT
          Not _ _ d -> compare c d
          _ -> LT
        Imp _ _ c e -> case b of
          Iff {} -> LT
          And {} -> LT
          Or {} -> LT
          Imp _ _ d f -> case compare c d of
            EQ -> compare e f
            z -> z
          _ -> GT
        Or _ _ c e -> case b of
          Iff {} -> LT
          And {} -> LT
          Or _ _ d f -> case compare c d of
            EQ -> compare e f
            z -> z
          _ -> GT
        And _ _ c e -> case b of
          Iff {} -> LT
          And _ _ d f -> case compare c d of
            EQ -> compare e f
            z -> z
          _ -> GT
        Iff _ _ c e -> case b of
          Iff _ _ d f -> case compare c d of
            EQ -> compare e f
            z -> z
          _ -> GT
      y -> y
    x -> x

instance PrettyPrintable a => PrettyPrintable (Formula a) where
  prettys = f False
    where
      f _ p@(Lit _) = g p
      f _ p@(Var _) = g p
      f _ p@Not {} = g p
      f b p = showParen b $ g p

      g (Lit False) = showString falseS
      g (Lit True) = showString trueS
      g (Var x) = prettys x
      g (Not _ _ p) = showString notS . f True p
      g (Imp _ _ p q) = f True p . padded impS . f True q
      g (Or _ _ p q) = f True p . padded orS . f True q
      g (And _ _ p q) = f True p . padded andS . f True q
      g (Iff _ _ p q) = f True p . padded iffS . f True q

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
    go s p@(Lit _) = Set.insert p s
    go s p@(Var _) = Set.insert p s
    go s p@(Not _ _ a) = go (Set.insert p s) a
    go s p@(Imp _ _ a b) = go (go (Set.insert p s) a) b
    go s p@(Or _ _ a b) = go (go (Set.insert p s) a) b
    go s p@(And _ _ a b) = go (go (Set.insert p s) a) b
    go s p@(Iff _ _ a b) = go (go (Set.insert p s) a) b

-- | @('substitute' a x p)@ represents \(a[x := p]\), the substitution of each
-- occurence of the variable \(x\) in the formula \(a\) by the formula \(p\).
--
-- ==== __Examples__
--
-- >>> substitute (var 'e' --> var 'e') 'e' (var 'a' /\ var 'a')
-- imp (and (var 'a') (var 'a')) (and (var 'a') (var 'a'))
substitute :: Eq a => Formula a -> a -> Formula a -> Formula a
substitute a@(Lit _) _ _ = a
substitute v@(Var y) x p = if x == y then p else v
substitute (Not _ _ a) x p = not $ substitute a x p
substitute (Imp _ _ a b) x p = imp (substitute a x p) (substitute b x p)
substitute (Or _ _ a b) x p = or (substitute a x p) (substitute b x p)
substitute (And _ _ a b) x p = and (substitute a x p) (substitute b x p)
substitute (Iff _ _ a b) x p = iff (substitute a x p) (substitute b x p)
