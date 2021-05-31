{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AutoProof.Formula.Types
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Formula' type, its constructors, and its instances.
module AutoProof.Formula.Types
  ( -- * Formula type
    Formula (Lit, Var, Not, Imp, Or, And, Iff),

    -- * Pretty-printing
    prettyFormula,
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
-- * disjunction \(\lor\),
-- * conjunction \(\land\), and
-- * equivalence \(\leftrightarrow\).
data Formula a
  = Lit_ !ASTMetadata Bool
  | Var_ !ASTMetadata a
  | Not_ !ASTMetadata !(Formula a)
  | Imp_ !ASTMetadata !(Formula a) !(Formula a)
  | Or_ !ASTMetadata !(Formula a) !(Formula a)
  | And_ !ASTMetadata !(Formula a) !(Formula a)
  | Iff_ !ASTMetadata !(Formula a) !(Formula a)

-- Smart constructors and pattern matching synonyms

-- | Propositional literal/constant. @('Lit' 'True')@ is \(\top\) (i.e., truth,
-- tautology, or top), and @('Lit' 'False')@ is \(\bot\) (i.e., falsity,
-- contradiction, or bottom).
pattern Lit :: Bool -> Formula a
pattern Lit x <-
  Lit_ _ x
  where
    Lit = atomicASTConstructor Lit_

-- | Propositional variable. @('Var' x)@ represents a propositional variable
-- \(x\).
pattern Var :: a -> Formula a
pattern Var x <-
  Var_ _ x
  where
    Var = atomicASTConstructor Var_

-- | Negation. @('Not' p)@ represents the formula \(\lnot p\).
pattern Not :: Formula a -> Formula a
pattern Not a <-
  Not_ _ a
  where
    Not = unaryASTConstructor Not_

-- | Implication. @('Imp' p q)@ represents the formula \(p \rightarrow q\).
pattern Imp :: Formula a -> Formula a -> Formula a
pattern Imp a b <-
  Imp_ _ a b
  where
    Imp = binaryASTConstructor Imp_

-- | Disjunction. @('Or' p q)@ represents the formula \(p \lor q\).
pattern Or :: Formula a -> Formula a -> Formula a
pattern Or a b <-
  Or_ _ a b
  where
    Or = binaryASTConstructor Or_

-- | Conjunction. @('And' p q)@ represents the formula \(p \land q\).
pattern And :: Formula a -> Formula a -> Formula a
pattern And a b <-
  And_ _ a b
  where
    And = binaryASTConstructor And_

-- | Equivalence. @('Iff' p q)@ represents the formula \(p \leftrightarrow q\).
pattern Iff :: Formula a -> Formula a -> Formula a
pattern Iff a b <-
  Iff_ _ a b
  where
    Iff = binaryASTConstructor Iff_

{-# COMPLETE Lit, Var, Not, Imp, Or, And, Iff #-}

instance AST (Formula a) where
  type Root (Formula a) = Maybe a

  root (Var x) = Just x
  root _ = Nothing

  children (Lit _) = []
  children (Var _) = []
  children (Not a) = [a]
  children (Imp a b) = [a, b]
  children (Or a b) = [a, b]
  children (And a b) = [a, b]
  children (Iff a b) = [a, b]

  metadata (Lit_ m _) = m
  metadata (Var_ m _) = m
  metadata (Not_ m _) = m
  metadata (Imp_ m _ _) = m
  metadata (Or_ m _ _) = m
  metadata (And_ m _ _) = m
  metadata (Iff_ m _ _) = m

-- Instance declarations

instance Show a => Show (Formula a) where
  showsPrec d = f (d > appPrec)
    where
      appPrec = 10

      f b a = showParen b $ g a

      g (Lit False) = showString "Lit False"
      g (Lit True) = showString "Lit True"
      g (Var x) = showString "Var " . showsPrec (appPrec + 1) x
      g (Not p) = showString "Not " . f True p
      g (Imp p q) = h "Imp " p q
      g (Or p q) = h "Or " p q
      g (And p q) = h "And " p q
      g (Iff p q) = h "Iff " p q

      h c p q = showString c . f True p . showString " " . f True q

instance Read a => Read (Formula a) where
  readsPrec d = f (d > appPrec)
    where
      appPrec = 10

      f b s = concatMap (($ s) . readParen b) readers
      readers =
        [ readLit,
          readVar,
          readNot,
          readImp,
          readOr,
          readAnd,
          readIff
        ]
      readLit s = [(Lit b, r) | ("Lit", t) <- lex s, (b, r) <- readsPrec (appPrec + 1) t]
      readVar s = [(Var x, r) | ("Var", t) <- lex s, (x, r) <- readsPrec (appPrec + 1) t]
      readNot = parseUnary Not "Not"
      readImp = parseBinary Imp "Imp"
      readOr = parseBinary Or "Or"
      readAnd = parseBinary And "And"
      readIff = parseBinary Iff "Iff"

      parseUnary c n s = [(c a, s'') | (n', s') <- lex s, n == n', (a, s'') <- f True s']
      parseBinary c n s = [(c a b, s''') | (n', s') <- lex s, n == n', (a, s'') <- f True s', (b, s''') <- f True s'']

-- | Syntactic equality.
instance Eq a => Eq (Formula a) where
  (Lit a) == (Lit b) = a == b
  (Var a) == (Var b) = a == b
  (Not a) == (Not b) = a == b
  (Imp a c) == (Imp b d) = a == b && c == d
  (Or a c) == (Or b d) = a == b && c == d
  (And a c) == (And b d) = a == b && c == d
  (Iff a c) == (Iff b d) = a == b && c == d
  _ == _ = False

-- | First compare heights, then compare sizes, then resolve using the
-- convention @Lit < Var < Not < Imp < Or < And < Iff@ (on equality, compare
-- children from left to right).
instance Ord a => Ord (Formula a) where
  compare a b = case compare (metadata a) (metadata b) of
    EQ -> case a of
      Lit c -> case b of
        Lit d -> compare c d
        _ -> LT
      Var c -> case b of
        Lit {} -> GT
        Var d -> compare c d
        _ -> LT
      Not c -> case b of
        Lit {} -> GT
        Var {} -> GT
        Not d -> compare c d
        _ -> LT
      Imp c e -> case b of
        Iff {} -> LT
        And {} -> LT
        Or {} -> LT
        Imp d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
      Or c e -> case b of
        Iff {} -> LT
        And {} -> LT
        Or d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
      And c e -> case b of
        Iff {} -> LT
        And d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
      Iff c e -> case b of
        Iff d f -> case compare c d of
          EQ -> compare e f
          y -> y
        _ -> GT
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
      g (Not p) = showString notS . f True p
      g (Imp p q) = f True p . padded impS . f True q
      g (Or p q) = f True p . padded orS . f True q
      g (And p q) = f True p . padded andS . f True q
      g (Iff p q) = f True p . padded iffS . f True q

      -- Pad a symbol with one space character on each side
      padded :: String -> ShowS
      padded s = showString (' ' : s) . showString " "

-- | Get a pretty-printed representation of a propositional formula.
--
-- ==== __Examples__
--
-- >>> prettyFormula $ Imp (Var 'a') (Or (Var 'b') (Lit False))
-- "a → (b ∨ ⊥)"
prettyFormula :: PrettyPrintable a => Formula a -> String
prettyFormula = pretty
