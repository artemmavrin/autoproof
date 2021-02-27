-- |
-- Module      : AutoProof.Proof.Types
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Proof' type.
module AutoProof.Proof.Types
  ( Proof (Ax, ImpElim, ImpIntr),
    Height,
    axiom,
    impElim,
    impIntr,
    height,
    judgement,
    premises,
    prettyProof,
  )
where

import AutoProof.Judgement (Judgement)
import AutoProof.Utils.PrettyPrintable (PrettyPrintable (pretty))
import AutoProof.Utils.Symbols (impS)
import Data.List (intercalate)

-- | A natural deduction proof tree for intuitionistic propositional logic.
--
-- Proofs can be created using the proof constructors
--
-- * 'axiom'
-- * 'impElim' (implication elimination, or /modus ponens/)
-- * 'impIntr' (implication introduction)
data Proof a
  = -- | Axiom.
    Ax !(Judgement a)
  | -- | Implication elimination
    ImpElim !Height !(Judgement a) !(Proof a) !(Proof a)
  | -- | Implication introduction
    ImpIntr !Height !(Judgement a) !(Proof a)

-- | Height of a proof
type Height = Int

-- | \(O(1)\). Get the height of a proof.
--
-- The /height/ of a proof is its height as a rooted tree (i.e., the number of
-- edges on the longest path from the root to a leaf).
height :: Proof a -> Int
height (Ax _) = 0
height (ImpElim d _ _ _) = d
height (ImpIntr d _ _) = d

-- | Get the final judgement of a proof.
judgement :: Proof a -> Judgement a
judgement (Ax j) = j
judgement (ImpElim _ j _ _) = j
judgement (ImpIntr _ j _) = j

-- | List of premises (as proofs) of a given proof.
premises :: Proof a -> [Proof a]
premises (Ax _) = []
premises (ImpElim _ _ p q) = [p, q]
premises (ImpIntr _ _ p) = [p]

-- | An axiom @('axiom' (g 'AutoProof.Judgement.|-' a))@ represents the
-- inference of the judgement \(g \vdash a\), where \(a \in g\):
--
-- \[
--   \frac{}{
--     g \vdash a
--   }
--   \, (\text{Ax})
-- \]
axiom :: Judgement a -> Proof a
axiom = Ax

-- | Implication elimination (/modus ponens/).
-- @('impElim' (g 'AutoProof.Judgement.|-' b) p q)@ represents the inference of
-- the judgement \(g \vdash b\) given a proof \(p\) of
-- \(g_1 \vdash a \rightarrow b\) and a proof \(q\) of \(g_2 \vdash q\), where
-- \(g_1 \cup g_2 \subseteq g\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g_1 \vdash a \rightarrow b
--     }
--     \qquad
--     \displaystyle\frac{
--       q
--     }{
--       g_2 \vdash a
--     }
--   }{
--     g \vdash b
--   }
--   \, ({\rightarrow}\text{E})
-- \]
impElim :: Judgement a -> Proof a -> Proof a -> Proof a
impElim j p q = ImpElim (1 + max (height p) (height q)) j p q

-- | Implication introduction.
-- @('impIntr' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.imp' a b) p)@
-- represents the inference of the judgement \(g \vdash a \rightarrow b\) given
-- a proof \(p\) of \(g, a \vdash b\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g, a \vdash b
--     }
--   }{
--     g \vdash a \rightarrow b
--   }
--   \, ({\rightarrow}\text{I})
-- \]
impIntr :: Judgement a -> Proof a -> Proof a
impIntr j p = ImpIntr (1 + height p) j p

-- Instance declarations

instance Eq a => Eq (Proof a) where
  (Ax j) == (Ax j') = j == j'
  (ImpElim _ j p q) == (ImpElim _ j' p' q') = j == j' && p == p' && q == q'
  (ImpIntr _ j p) == (ImpIntr _ j' p') = j == j' && p == p'
  _ == _ = False

instance Ord a => Ord (Proof a) where
  compare p p' = case compare (height p) (height p') of
    EQ -> case p of
      Ax j -> case p' of
        Ax j' -> compare j j'
        _ -> LT
      ImpElim _ j q r -> case p' of
        Ax _ -> GT
        ImpElim _ j' q' r' -> binary j q r j' q' r'
        _ -> LT
      ImpIntr _ j q -> case p' of
        Ax _ -> GT
        ImpElim {} -> GT
        ImpIntr _ j' q' -> unary j q j' q'
    x -> x
    where
      unary j q j' q' = case compare j j' of
        EQ -> compare q q'
        x -> x
      binary j q r j' q' r' = case compare j j' of
        EQ -> case compare q q' of
          EQ -> compare r r'
          y -> y
        x -> x

instance Show a => Show (Proof a) where
  showsPrec d = f (d > appPrec)
    where
      appPrec = 10

      f b a = showParen b $ g a

      g (Ax j) = nullary "axiom " j
      g (ImpElim _ j p q) = binary "impElim " j p q
      g (ImpIntr _ j p) = unary "impIntr " j p

      nullary c j = showString c . showsPrec (appPrec + 1) j

      unary c j p =
        showString c . showsPrec (appPrec + 1) j
          . showString " "
          . f True p

      binary c j p q =
        showString c . showsPrec (appPrec + 1) j
          . showString " "
          . f True p
          . showString " "
          . f True q

-- TODO: clean this instance up
instance PrettyPrintable a => PrettyPrintable (Proof a) where
  pretty = concatIndent . proofLines (0 :: Int)
    where
      proofLines level p =
        [ (pretty $ judgement p, level),
          (name p, level + 1)
        ]
          ++ (premises p >>= proofLines (level + 1))

      concatIndent l = intercalate "\n" $ indent <$> l

      indent (s, 0) = s
      indent (s, n) = "    " ++ indent (s, n - 1)

      name Ax {} = "(Ax)"
      name ImpElim {} = "(" ++ impS ++ "E)"
      name ImpIntr {} = "(" ++ impS ++ "I)"

-- | Get a pretty-printed representation of a proof.
prettyProof :: PrettyPrintable a => Proof a -> String
prettyProof = pretty
