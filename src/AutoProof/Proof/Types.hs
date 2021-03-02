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
  ( Proof
      ( Ax,
        FalseElim,
        TrueIntr,
        NotElim,
        NotIntr,
        ImpElim,
        ImpIntr
      ),
    Height,
    axiom,
    falseElim,
    trueIntr,
    notElim,
    notIntr,
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
import AutoProof.Utils.Symbols
  ( axiomS,
    branchS,
    cornerS,
    falseElimS,
    impElimS,
    impIntrS,
    notElimS,
    notIntrS,
    trueIntrS,
    vertS,
  )

-- | A natural deduction proof tree for intuitionistic propositional logic.
--
-- Proofs can be created using the proof constructors
--
-- * 'axiom'
-- * 'falseElim' (falsity elimination, or the principle of explosion)
-- * 'trueIntr' (truth introduction)
-- * 'notElim' (negation elimination)
-- * 'notIntr' (negation introduction)
-- * 'impElim' (implication elimination, or /modus ponens/)
-- * 'impIntr' (implication introduction)
data Proof a
  = -- | Axiom.
    Ax !(Judgement a)
  | -- | Bottom elimination.
    FalseElim !Height !(Judgement a) (Proof a)
  | -- | Truth introduction
    TrueIntr !(Judgement a)
  | -- | Negation elimination
    NotElim !Height !(Judgement a) !(Proof a) !(Proof a)
  | -- | Negation introduction
    NotIntr !Height !(Judgement a) !(Proof a)
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
height (FalseElim h _ _) = h
height (TrueIntr _) = 0
height (NotElim h _ _ _) = h
height (NotIntr h _ _) = h
height (ImpElim h _ _ _) = h
height (ImpIntr h _ _) = h

-- | Get the final judgement of a proof.
judgement :: Proof a -> Judgement a
judgement (Ax j) = j
judgement (FalseElim _ j _) = j
judgement (TrueIntr j) = j
judgement (NotElim _ j _ _) = j
judgement (NotIntr _ j _) = j
judgement (ImpElim _ j _ _) = j
judgement (ImpIntr _ j _) = j

-- | List of premises (as proofs) of a given proof.
premises :: Proof a -> [Proof a]
premises (Ax _) = []
premises (FalseElim _ _ p) = [p]
premises (TrueIntr _) = []
premises (NotElim _ _ p q) = [p, q]
premises (NotIntr _ _ p) = [p]
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

-- | Falsity elimination (principle of explosion).
-- @('falseElim' (g 'AutoProof.Judgement.|-' a) p)@ represents the inference of
-- the judgement \(g \vdash a\) given a proof \(p\) of \(g \vdash \bot\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g \vdash \bot
--     }
--   }{
--     g \vdash a
--   }
--   \, ({\bot}\text{E})
-- \]
falseElim :: Judgement a -> Proof a -> Proof a
falseElim = unaryConstructor FalseElim

-- | Truth introduction.
-- @('trueIntr' (g 'AutoProof.Judgement.|-' 'AutoProof.Formula.true'))@
-- represents the vacuous inference of the judgement \(g \vdash \top\):
--
-- \[
--   \frac{}{
--     g \vdash \top
--   }
--   \, (\top\text{I})
-- \]
trueIntr :: Judgement a -> Proof a
trueIntr = TrueIntr

-- | Negation elimination.
-- @('notElim' (g 'AutoProof.Judgement.|-' 'AutoProof.Formula.false') p q)@
-- represents the inference of the judgement \(g \vdash \bot\) given a proof
-- \(p\) of \(g_1 \vdash \lnot a\) and a proof \(q\) of \(g_2 \vdash a\), where
-- \(g_1 \cup g_2 \subseteq g\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g_1 \vdash \lnot a
--     }
--     \qquad
--     \displaystyle\frac{
--       q
--     }{
--       g_2 \vdash a
--     }
--   }{
--     g \vdash \bot
--   }
--   \, ({\lnot}\text{E})
-- \]
notElim :: Judgement a -> Proof a -> Proof a -> Proof a
notElim = binaryConstructor ImpElim

-- | Negation introduction.
-- @('notIntr' (g 'AutoProof.Judgement.|-' ('not' a)) p)@ represents the
-- inference of the judgement \(g \vdash \lnot a\) given a proof \(p\) of
-- \(g, a \vdash \bot\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g, a \vdash \bot
--     }
--   }{
--     g \vdash \lnot a
--   }
--   \, ({\lnot}\text{I})
-- \]
notIntr :: Judgement a -> Proof a -> Proof a
notIntr = unaryConstructor ImpIntr

-- | Implication elimination (/modus ponens/).
-- @('impElim' (g 'AutoProof.Judgement.|-' b) p q)@ represents the inference of
-- the judgement \(g \vdash b\) given a proof \(p\) of
-- \(g_1 \vdash a \rightarrow b\) and a proof \(q\) of \(g_2 \vdash a\), where
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
impElim = binaryConstructor ImpElim

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
impIntr = unaryConstructor ImpIntr

unaryConstructor ::
  (Height -> Judgement a -> Proof a -> Proof a) ->
  Judgement a ->
  Proof a ->
  Proof a
unaryConstructor c j p = c (1 + height p) j p

binaryConstructor ::
  (Height -> Judgement a -> Proof a -> Proof a -> Proof a) ->
  Judgement a ->
  Proof a ->
  Proof a ->
  Proof a
binaryConstructor c j p q = c (1 + max (height p) (height q)) j p q

-- Instance declarations

instance Eq a => Eq (Proof a) where
  (Ax j) == (Ax j') = j == j'
  (FalseElim _ j p) == (FalseElim _ j' p') = j == j' && p == p'
  (TrueIntr j) == (TrueIntr j') = j == j'
  (NotElim _ j p q) == (NotElim _ j' p' q') = j == j' && p == p' && q == q'
  (NotIntr _ j p) == (NotIntr _ j' p') = j == j' && p == p'
  (ImpElim _ j p q) == (ImpElim _ j' p' q') = j == j' && p == p' && q == q'
  (ImpIntr _ j p) == (ImpIntr _ j' p') = j == j' && p == p'
  _ == _ = False

instance Ord a => Ord (Proof a) where
  compare p p' = case compare (height p) (height p') of
    EQ -> case p of
      Ax j -> case p' of
        Ax j' -> compare j j'
        _ -> LT
      FalseElim _ j q -> case p' of
        Ax {} -> GT
        FalseElim _ j' q' -> compareUnary j q j' q'
        _ -> LT
      TrueIntr j -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr j' -> compare j j'
        _ -> LT
      NotElim _ j q r -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim _ j' q' r' -> compareBinary j q r j' q' r'
        _ -> LT
      NotIntr _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr _ j' q' -> compareUnary j q j' q'
        _ -> LT
      ImpElim _ j q r -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim _ j' q' r' -> compareBinary j q r j' q' r'
        _ -> LT
      ImpIntr _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr _ j' q' -> compareUnary j q j' q'
    x -> x
    where
      compareUnary j q j' q' = case compare j j' of
        EQ -> compare q q'
        x -> x
      compareBinary j q r j' q' r' = case compare j j' of
        EQ -> case compare q q' of
          EQ -> compare r r'
          y -> y
        x -> x

instance Show a => Show (Proof a) where
  showsPrec d = f (d > appPrec)
    where
      appPrec = 10

      f b a = showParen b $ g a

      g (Ax j) = showsNullary "axiom " j
      g (FalseElim _ j p) = showsUnary "falseElim " j p
      g (TrueIntr j) = showsNullary "trueIntr " j
      g (NotElim _ j p q) = showsBinary "notElim " j p q
      g (NotIntr _ j p) = showsUnary "notIntr " j p
      g (ImpElim _ j p q) = showsBinary "impElim " j p q
      g (ImpIntr _ j p) = showsUnary "impIntr " j p

      showsNullary c j = showString c . showsPrec (appPrec + 1) j

      showsUnary c j p =
        showString c . showsPrec (appPrec + 1) j
          . showString " "
          . f True p

      showsBinary c j p q =
        showString c . showsPrec (appPrec + 1) j
          . showString " "
          . f True p
          . showString " "
          . f True q

instance PrettyPrintable a => PrettyPrintable (Proof a) where
  -- Adapted from drawTree in Data.Tree from the containers package
  pretty = unlines . reverse . draw
    where
      draw p = line p : drawPremises (reverse (premises p))

      line p = rule p ++ " " ++ pretty (judgement p)

      drawPremises [] = []
      drawPremises [q] =
        vertS : shift cornerS "    " (draw q)
      drawPremises (q : qs) =
        vertS : shift branchS (vertS ++ "   ") (draw q) ++ drawPremises qs

      shift first other = zipWith (++) (first : repeat other)

      rule Ax {} = axiomS
      rule FalseElim {} = falseElimS
      rule TrueIntr {} = trueIntrS
      rule NotElim {} = notElimS
      rule NotIntr {} = notIntrS
      rule ImpElim {} = impElimS
      rule ImpIntr {} = impIntrS

-- | Get a pretty-printed representation of a proof.
prettyProof :: PrettyPrintable a => Proof a -> String
prettyProof = pretty
