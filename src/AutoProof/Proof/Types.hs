{-# LANGUAGE TypeFamilies #-}

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
    axioms,
    prettyProof,
  )
where

import AutoProof.AST
  ( AST (Root, children, height, metadata, root),
    ASTMetadata,
    atomicASTConstructor,
    binaryRootedASTConstructor,
    ternaryRootedASTConstructor,
    unaryRootedASTConstructor,
  )
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

-- | A proof tree for intuitionistic propositional logic.
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
  = -- | Axiom. Use 'axiom'.
    Ax !ASTMetadata (Judgement a)
  | -- | Bottom elimination. Use 'falseElim'.
    FalseElim !ASTMetadata (Judgement a) (Proof a)
  | -- | Truth introduction. Use 'trueIntr'.
    TrueIntr !ASTMetadata (Judgement a)
  | -- | Negation elimination. Use 'notElim'.
    NotElim !ASTMetadata (Judgement a) !(Proof a) !(Proof a)
  | -- | Negation introduction. Use 'notIntr'.
    NotIntr !ASTMetadata (Judgement a) !(Proof a)
  | -- | Implication elimination. Use 'impElim'.
    ImpElim !ASTMetadata (Judgement a) !(Proof a) !(Proof a)
  | -- | Implication introduction. Use 'impIntr'.
    ImpIntr !ASTMetadata (Judgement a) !(Proof a)
  | -- | Disjunction elimination. Use 'orElim'.
    OrElim !ASTMetadata (Judgement a) !(Proof a) !(Proof a) !(Proof a)
  | -- | Left disjunction introduction. Use 'orIntrL'.
    OrIntrL !ASTMetadata (Judgement a) !(Proof a)
  | -- | Right disjunction introduction. Use 'orIntrR'.
    OrIntrR !ASTMetadata (Judgement a) !(Proof a)
  | -- | Left conjunction elimination. Use 'andElimL'.
    AndElimL !ASTMetadata (Judgement a) !(Proof a)
  | -- | Right conjunction elimination. Use 'andElimR'.
    AndElimR !ASTMetadata (Judgement a) !(Proof a)
  | -- | Conjunction introduction. Use 'andIntr'.
    AndIntr !ASTMetadata (Judgement a) !(Proof a) !(Proof a)
  | -- | Left equivalence elimination. Use 'iffElimL'.
    IffElimL !ASTMetadata (Judgement a) !(Proof a)
  | -- | Right equivalence elimination. Use 'iffElimR'.
    IffElimR !ASTMetadata (Judgement a) !(Proof a)
  | -- | Equivalence introduction. Use 'iffIntr'.
    IffIntr !ASTMetadata (Judgement a) !(Proof a) !(Proof a)

instance AST (Proof a) where
  type Root (Proof a) = Judgement a

  root (Ax _ j) = j
  root (FalseElim _ j _) = j
  root (TrueIntr _ j) = j
  root (NotElim _ j _ _) = j
  root (NotIntr _ j _) = j
  root (ImpElim _ j _ _) = j
  root (ImpIntr _ j _) = j
  root (OrElim _ j _ _ _) = j
  root (OrIntrL _ j _) = j
  root (OrIntrR _ j _) = j
  root (AndElimL _ j _) = j
  root (AndElimR _ j _) = j
  root (AndIntr _ j _ _) = j
  root (IffElimL _ j _) = j
  root (IffElimR _ j _) = j
  root (IffIntr _ j _ _) = j

  children (Ax _ _) = []
  children (FalseElim _ _ p) = [p]
  children (TrueIntr _ _) = []
  children (NotElim _ _ p q) = [p, q]
  children (NotIntr _ _ p) = [p]
  children (ImpElim _ _ p q) = [p, q]
  children (ImpIntr _ _ p) = [p]
  children (OrElim _ _ p q r) = [p, q, r]
  children (OrIntrL _ _ p) = [p]
  children (OrIntrR _ _ p) = [p]
  children (AndElimL _ _ p) = [p]
  children (AndElimR _ _ p) = [p]
  children (AndIntr _ _ p q) = [p, q]
  children (IffElimL _ _ p) = [p]
  children (IffElimR _ _ p) = [p]
  children (IffIntr _ _ p q) = [p, q]

  metadata (Ax m _) = m
  metadata (FalseElim m _ _) = m
  metadata (TrueIntr m _) = m
  metadata (NotElim m _ _ _) = m
  metadata (NotIntr m _ _) = m
  metadata (ImpElim m _ _ _) = m
  metadata (ImpIntr m _ _) = m
  metadata (OrElim m _ _ _ _) = m
  metadata (OrIntrL m _ _) = m
  metadata (OrIntrR m _ _) = m
  metadata (AndElimL m _ _) = m
  metadata (AndElimR m _ _) = m
  metadata (AndIntr m _ _ _) = m
  metadata (IffElimL m _ _) = m
  metadata (IffElimR m _ _) = m
  metadata (IffIntr m _ _ _) = m

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
axiom = atomicASTConstructor Ax

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
falseElim = unaryRootedASTConstructor FalseElim

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
trueIntr = atomicASTConstructor TrueIntr

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
notElim = binaryRootedASTConstructor NotElim

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
notIntr = unaryRootedASTConstructor NotIntr

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
impElim = binaryRootedASTConstructor ImpElim

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
impIntr = unaryRootedASTConstructor ImpIntr

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
orElim = ternaryRootedASTConstructor OrElim

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
orIntrL = unaryRootedASTConstructor OrIntrL

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
orIntrR = unaryRootedASTConstructor OrIntrR

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
andElimL = unaryRootedASTConstructor AndElimL

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
andElimR = unaryRootedASTConstructor AndElimR

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
andIntr = binaryRootedASTConstructor AndIntr

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
iffElimL = unaryRootedASTConstructor IffElimL

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
iffElimR = unaryRootedASTConstructor IffElimR

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
iffIntr = binaryRootedASTConstructor IffIntr

-- Miscellaneous proof operations

-- | Get the set of axioms used in a proof.
axioms :: Ord a => Proof a -> Set (Formula a)
axioms = go Set.empty
  where
    go s (Ax _ (Judgement _ a)) = Set.insert a s
    go s (FalseElim _ _ p) = go s p
    go s (TrueIntr _ _) = s
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
  (Ax _ j) == (Ax _ j') = j == j'
  (FalseElim _ j p) == (FalseElim _ j' p') = j == j' && p == p'
  (TrueIntr _ j) == (TrueIntr _ j') = j == j'
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
      Ax _ j -> case p' of
        Ax _ j' -> compare j j'
        _ -> LT
      FalseElim _ j q -> case p' of
        Ax {} -> GT
        FalseElim _ j' q' -> compareUnary j q j' q'
        _ -> LT
      TrueIntr _ j -> case p' of
        Ax {} -> GT
        FalseElim {} -> GT
        TrueIntr _ j' -> compare j j'
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

      g (Ax _ j) = showsNullary "axiom " j
      g (FalseElim _ j p) = showsUnary "falseElim " j p
      g (TrueIntr _ j) = showsNullary "trueIntr " j
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
      draw p = line p : drawPremises (reverse (children p))

      line p = rule p ++ " " ++ pretty (root p)

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
