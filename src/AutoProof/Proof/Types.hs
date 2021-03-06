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
        ImpIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        AndElimL,
        AndElimR,
        AndIntr,
        IffElimL,
        IffElimR,
        IffIntr
      ),
    Height,
    axiom,
    falseElim,
    trueIntr,
    notElim,
    notIntr,
    impElim,
    impIntr,
    orElim,
    orIntrL,
    orIntrR,
    andElimL,
    andElimR,
    andIntr,
    iffElimL,
    iffElimR,
    iffIntr,
    height,
    judgement,
    premises,
    axioms,
    prettyProof,
  )
where

import AutoProof.Formula (Formula)
import AutoProof.Judgement (Judgement (Judgement))
import AutoProof.Utils.PrettyPrintable (PrettyPrintable (pretty))
import AutoProof.Utils.Symbols
  ( andElimLS,
    andElimRS,
    andIntrS,
    axiomS,
    branchS,
    cornerS,
    falseElimS,
    iffElimLS,
    iffElimRS,
    iffIntrS,
    impElimS,
    impIntrS,
    notElimS,
    notIntrS,
    orElimS,
    orIntrLS,
    orIntrRS,
    trueIntrS,
    vertS,
  )
import Data.Set (Set)
import qualified Data.Set as Set

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
-- * 'orElim' (disjunction elimination)
-- * 'orIntrL' (left disjunction introduction)
-- * 'orIntrR' (right disjunction introduction)
-- * 'andElimL' (left conjunction elimination)
-- * 'andElimR' (right conjunction elimination)
-- * 'andIntr' (conjunction introduction)
-- * 'iffElimL' (left equivalence elimination)
-- * 'iffElimR' (right equivalence elimination)
-- * 'iffIntr' (equivalence introduction)
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
  | -- | Disjunction elimination
    OrElim !Height !(Judgement a) !(Proof a) !(Proof a) !(Proof a)
  | -- | Left disjunction introduction
    OrIntrL !Height !(Judgement a) !(Proof a)
  | -- | Right disjunction introduction
    OrIntrR !Height !(Judgement a) !(Proof a)
  | -- | Left conjunction introduction
    AndElimL !Height !(Judgement a) !(Proof a)
  | -- | Right conjunction introduction
    AndElimR !Height !(Judgement a) !(Proof a)
  | -- | Conjunction introduction
    AndIntr !Height !(Judgement a) !(Proof a) !(Proof a)
  | -- | Left equivalence introduction
    IffElimL !Height !(Judgement a) !(Proof a)
  | -- | Right equivalence introduction
    IffElimR !Height !(Judgement a) !(Proof a)
  | -- | Equivalence introduction
    IffIntr !Height !(Judgement a) !(Proof a) !(Proof a)

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
height (OrElim h _ _ _ _) = h
height (OrIntrL h _ _) = h
height (OrIntrR h _ _) = h
height (AndElimL h _ _) = h
height (AndElimR h _ _) = h
height (AndIntr h _ _ _) = h
height (IffElimL h _ _) = h
height (IffElimR h _ _) = h
height (IffIntr h _ _ _) = h

-- | Get the final judgement of a proof.
judgement :: Proof a -> Judgement a
judgement (Ax j) = j
judgement (FalseElim _ j _) = j
judgement (TrueIntr j) = j
judgement (NotElim _ j _ _) = j
judgement (NotIntr _ j _) = j
judgement (ImpElim _ j _ _) = j
judgement (ImpIntr _ j _) = j
judgement (OrElim _ j _ _ _) = j
judgement (OrIntrL _ j _) = j
judgement (OrIntrR _ j _) = j
judgement (AndElimL _ j _) = j
judgement (AndElimR _ j _) = j
judgement (AndIntr _ j _ _) = j
judgement (IffElimL _ j _) = j
judgement (IffElimR _ j _) = j
judgement (IffIntr _ j _ _) = j

-- | List of premises (as proofs) of a given proof.
premises :: Proof a -> [Proof a]
premises (Ax _) = []
premises (FalseElim _ _ p) = [p]
premises (TrueIntr _) = []
premises (NotElim _ _ p q) = [p, q]
premises (NotIntr _ _ p) = [p]
premises (ImpElim _ _ p q) = [p, q]
premises (ImpIntr _ _ p) = [p]
premises (OrElim _ _ p q r) = [p, q, r]
premises (OrIntrL _ _ p) = [p]
premises (OrIntrR _ _ p) = [p]
premises (AndElimL _ _ p) = [p]
premises (AndElimR _ _ p) = [p]
premises (AndIntr _ _ p q) = [p, q]
premises (IffElimL _ _ p) = [p]
premises (IffElimR _ _ p) = [p]
premises (IffIntr _ _ p q) = [p, q]

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
-- \(g = g_1 \cup g_2\):
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
notElim = binaryConstructor NotElim

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
notIntr = unaryConstructor NotIntr

-- | Implication elimination (/modus ponens/).
-- @('impElim' (g 'AutoProof.Judgement.|-' b) p q)@ represents the inference of
-- the judgement \(g \vdash b\) given a proof \(p\) of
-- \(g_1 \vdash a \rightarrow b\) and a proof \(q\) of \(g_2 \vdash a\), where
-- \(g = g_1 \cup g_2\):
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

-- | Disjunction elimination.
-- @('orElim' (g 'AutoProof.Judgement.|-' c) p q r)@ represents the inference of
-- the judgement \(g \vdash c\) given proofs \(p\) of \(g_1 \vdash a \lor b\),
-- \(q\) of \(g_2, a \vdash c\), and \(r\) of \(g_3, b \vdash c\), where
-- \(g = g_1 \cup g_2 \cup g_3\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g_1 \vdash a \lor b
--     }
--     \qquad
--     \displaystyle\frac{
--       q
--     }{
--       g_2, a \vdash c
--     }
--     \qquad
--     \displaystyle\frac{
--       r
--     }{
--       g_3, b \vdash c
--     }
--   }{
--     g \vdash c
--   }
--   \, ({\lor}\text{E})
-- \]
orElim :: Judgement a -> Proof a -> Proof a -> Proof a -> Proof a
orElim = ternaryConstructor OrElim

-- | Left disjunction introduction.
-- @('orIntrL' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.or' a b)) p)@
-- represents the inference of the judgement \(g \vdash a \lor b\) given a proof
-- \(p\) of \(g \vdash a\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g \vdash a
--     }
--   }{
--     g \vdash a \lor b
--   }
--   \, ({\lor}\text{IL})
-- \]
orIntrL :: Judgement a -> Proof a -> Proof a
orIntrL = unaryConstructor OrIntrL

-- | Right disjunction introduction.
-- @('orIntrR' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.or' a b)) p)@
-- represents the inference of the judgement \(g \vdash a \lor b\) given a proof
-- \(p\) of \(g \vdash b\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g \vdash b
--     }
--   }{
--     g \vdash a \lor b
--   }
--   \, ({\lor}\text{IR})
-- \]
orIntrR :: Judgement a -> Proof a -> Proof a
orIntrR = unaryConstructor OrIntrR

-- | Left conjunction elimination.
-- @('andElimL' (g 'AutoProof.Judgement.|-' a) p)@ represents the inference of
-- the judgement \(g \vdash a\) given a proof \(p\) of \(g \vdash a \land b\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g \vdash a \land b
--     }
--   }{
--     g \vdash a
--   }
--   \, ({\land}\text{EL})
-- \]
andElimL :: Judgement a -> Proof a -> Proof a
andElimL = unaryConstructor AndElimL

-- | Right conjunction elimination.
-- @('andElimR' (g 'AutoProof.Judgement.|-' b) p)@ represents the inference of
-- the judgement \(g \vdash b\) given a proof \(p\) of \(g \vdash a \land b\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g \vdash a \land b
--     }
--   }{
--     g \vdash b
--   }
--   \, ({\land}\text{ER})
-- \]
andElimR :: Judgement a -> Proof a -> Proof a
andElimR = unaryConstructor AndElimR

-- | Conjunction introduction.
-- @('andIntr' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.and' a b)) p)@
-- represents the inference of the judgement \(g \vdash a \land b\) given a
-- proof \(p\) of \(g_1 \vdash a\) and a proof \(q\) of \(g_2 \vdash b\), where
-- \(g = g_1 \cup g_2\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g_1 \vdash a
--     }
--     \qquad
--     \displaystyle\frac{
--       q
--     }{
--       g_2 \vdash b
--     }
--   }{
--     g \vdash a \land b
--   }
--   \, ({\land}\text{I})
-- \]
andIntr :: Judgement a -> Proof a -> Proof a -> Proof a
andIntr = binaryConstructor AndIntr

-- | Left equivalence elimination.
-- @('iffElimL' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.imp' a b)) p)@
-- represents the inference of the judgement \(g \vdash a \rightarrow b\) given
-- a proof \(p\) of \(g \vdash a \leftrightarrow b\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g \vdash a \leftrightarrow b
--     }
--   }{
--     g \vdash a \rightarrow b
--   }
--   \, ({\leftrightarrow}\text{EL})
-- \]
iffElimL :: Judgement a -> Proof a -> Proof a
iffElimL = unaryConstructor IffElimL

-- | Right equivalence elimination.
-- @('iffElimR' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.imp' b a)) p)@
-- represents the inference of the judgement \(g \vdash b \rightarrow a\) given
-- a proof \(p\) of \(g \vdash a \leftrightarrow b\):
--
-- \[
--   \frac{
--     \displaystyle\frac{
--       p
--     }{
--       g \vdash a \leftrightarrow b
--     }
--   }{
--     g \vdash b \rightarrow a
--   }
--   \, ({\leftrightarrow}\text{ER})
-- \]
iffElimR :: Judgement a -> Proof a -> Proof a
iffElimR = unaryConstructor IffElimR

-- | Equivalence introduction.
-- @('iffIntr' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.iff' a b)) p)@
-- represents the inference of the judgement \(g \vdash a \leftrightarrow b\)
-- given a proof \(p\) of \(g_1 \vdash a \rightarrow b\) and a proof \(q\) of
-- \(g_2 \vdash b \rightarrow a\), where \(g = g_1 \cup g_2\):
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
--       g_2 \vdash b \rightarrow a
--     }
--   }{
--     g \vdash a \leftrightarrow b
--   }
--   \, ({\leftrightarrow}\text{I})
-- \]
iffIntr :: Judgement a -> Proof a -> Proof a -> Proof a
iffIntr = binaryConstructor IffIntr

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

ternaryConstructor ::
  (Height -> Judgement a -> Proof a -> Proof a -> Proof a -> Proof a) ->
  Judgement a ->
  Proof a ->
  Proof a ->
  Proof a ->
  Proof a
ternaryConstructor c j p q r = c (1 + max (height p) (max (height q) (height r))) j p q r

-- Miscellaneous proof operations

-- | Get the set of axioms used in a proof.
axioms :: Ord a => Proof a -> Set (Formula a)
axioms = go Set.empty
  where
    go s (Ax (Judgement _ a)) = Set.insert a s
    go s (FalseElim _ _ p) = go s p
    go s (TrueIntr _) = s
    go s (NotElim _ _ p q) = go (go s p) q
    go s (NotIntr _ _ p) = go s p
    go s (ImpElim _ _ p q) = go (go s p) q
    go s (ImpIntr _ _ p) = go s p
    go s (OrElim _ _ p q r) = go (go (go s p) q) r
    go s (OrIntrL _ _ p) = go s p
    go s (OrIntrR _ _ p) = go s p
    go s (AndElimL _ _ p) = go s p
    go s (AndElimR _ _ p) = go s p
    go s (AndIntr _ _ p q) = go (go s p) q
    go s (IffElimL _ _ p) = go s p
    go s (IffElimR _ _ p) = go s p
    go s (IffIntr _ _ p q) = go (go s p) q

-- Instance declarations

instance Eq a => Eq (Proof a) where
  (Ax j) == (Ax j') = j == j'
  (FalseElim _ j p) == (FalseElim _ j' p') = j == j' && p == p'
  (TrueIntr j) == (TrueIntr j') = j == j'
  (NotElim _ j p q) == (NotElim _ j' p' q') = j == j' && p == p' && q == q'
  (NotIntr _ j p) == (NotIntr _ j' p') = j == j' && p == p'
  (ImpElim _ j p q) == (ImpElim _ j' p' q') = j == j' && p == p' && q == q'
  (ImpIntr _ j p) == (ImpIntr _ j' p') = j == j' && p == p'
  (OrElim _ j p q r) == (OrElim _ j' p' q' r') = j == j' && p == p' && q == q' && r == r'
  (OrIntrL _ j p) == (OrIntrL _ j' p') = j == j' && p == p'
  (OrIntrR _ j p) == (OrIntrR _ j' p') = j == j' && p == p'
  (AndElimL _ j p) == (AndElimL _ j' p') = j == j' && p == p'
  (AndElimR _ j p) == (AndElimR _ j' p') = j == j' && p == p'
  (AndIntr _ j p q) == (AndIntr _ j' p' q') = j == j' && p == p' && q == q'
  (IffElimL _ j p) == (IffElimL _ j' p') = j == j' && p == p'
  (IffElimR _ j p) == (IffElimR _ j' p') = j == j' && p == p'
  (IffIntr _ j p q) == (IffIntr _ j' p' q') = j == j' && p == p' && q == q'
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
        _ -> LT
      OrElim _ j q r s -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim _ j' q' r' s' -> compareTernary j q r s j' q' r' s'
        _ -> LT
      OrIntrL _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL _ j' q' -> compareUnary j q j' q'
        _ -> LT
      OrIntrR _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR _ j' q' -> compareUnary j q j' q'
        _ -> LT
      AndElimL _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR {} -> GT
        AndElimL _ j' q' -> compareUnary j q j' q'
        _ -> LT
      AndElimR _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR {} -> GT
        AndElimL {} -> GT
        AndElimR _ j' q' -> compareUnary j q j' q'
        _ -> LT
      AndIntr _ j q r -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR {} -> GT
        AndElimL {} -> GT
        AndElimR {} -> GT
        AndIntr _ j' q' r' -> compareBinary j q r j' q' r'
        _ -> LT
      IffElimL _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR {} -> GT
        AndElimL {} -> GT
        AndElimR {} -> GT
        AndIntr {} -> GT
        IffElimL _ j' q' -> compareUnary j q j' q'
        _ -> LT
      IffElimR _ j q -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR {} -> GT
        AndElimL {} -> GT
        AndElimR {} -> GT
        AndIntr {} -> GT
        IffElimL {} -> GT
        IffElimR _ j' q' -> compareUnary j q j' q'
        _ -> LT
      IffIntr _ j q r -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR {} -> GT
        AndElimL {} -> GT
        AndElimR {} -> GT
        AndIntr {} -> GT
        IffElimL {} -> GT
        IffElimR {} -> GT
        IffIntr _ j' q' r' -> compareBinary j q r j' q' r'
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
      compareTernary j q r s j' q' r' s' = case compare j j' of
        EQ -> case compare q q' of
          EQ -> case compare r r' of
            EQ -> compare s s'
            z -> z
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
      g (OrElim _ j p q r) = showsTernary "orElim " j p q r
      g (OrIntrL _ j p) = showsUnary "orIntrL " j p
      g (OrIntrR _ j p) = showsUnary "orIntrR " j p
      g (AndElimL _ j p) = showsUnary "andElimL " j p
      g (AndElimR _ j p) = showsUnary "andElimR " j p
      g (AndIntr _ j p q) = showsBinary "andIntr " j p q
      g (IffElimL _ j p) = showsUnary "iffElimL " j p
      g (IffElimR _ j p) = showsUnary "iffElimR " j p
      g (IffIntr _ j p q) = showsBinary "iffIntr " j p q

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

      showsTernary c j p q r =
        showString c . showsPrec (appPrec + 1) j
          . showString " "
          . f True p
          . showString " "
          . f True q
          . showString " "
          . f True r

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
      rule OrElim {} = orElimS
      rule OrIntrL {} = orIntrLS
      rule OrIntrR {} = orIntrRS
      rule AndElimL {} = andElimLS
      rule AndElimR {} = andElimRS
      rule AndIntr {} = andIntrS
      rule IffElimL {} = iffElimLS
      rule IffElimR {} = iffElimRS
      rule IffIntr {} = iffIntrS

-- | Get a pretty-printed representation of a proof.
prettyProof :: PrettyPrintable a => Proof a -> String
prettyProof = pretty
