{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AutoProof.Internal.Proof.Types
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'Proof' type.
module AutoProof.Internal.Proof.Types
  ( Proof
      ( Axiom,
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
    prettyProof,
  )
where

import AutoProof.Internal.AST
  ( AST (Root, children, height, metadata, root),
    ASTMetadata,
    atomicASTConstructor,
    binaryRootedASTConstructor,
    ternaryRootedASTConstructor,
    unaryRootedASTConstructor,
  )
import AutoProof.Internal.Judgement (Judgement)
import AutoProof.Internal.Utils.PrettyPrintable (PrettyPrintable (pretty))
import AutoProof.Internal.Utils.PrettyPrintable.Symbols
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

-- | A proof tree for propositional logic.
data Proof a
  = Axiom_ !ASTMetadata (Judgement a)
  | FalseElim_ !ASTMetadata (Judgement a) (Proof a)
  | TrueIntr_ !ASTMetadata (Judgement a)
  | NotElim_ !ASTMetadata (Judgement a) !(Proof a) !(Proof a)
  | NotIntr_ !ASTMetadata (Judgement a) !(Proof a)
  | ImpElim_ !ASTMetadata (Judgement a) !(Proof a) !(Proof a)
  | ImpIntr_ !ASTMetadata (Judgement a) !(Proof a)
  | OrElim_ !ASTMetadata (Judgement a) !(Proof a) !(Proof a) !(Proof a)
  | OrIntrL_ !ASTMetadata (Judgement a) !(Proof a)
  | OrIntrR_ !ASTMetadata (Judgement a) !(Proof a)
  | AndElimL_ !ASTMetadata (Judgement a) !(Proof a)
  | AndElimR_ !ASTMetadata (Judgement a) !(Proof a)
  | AndIntr_ !ASTMetadata (Judgement a) !(Proof a) !(Proof a)
  | IffElimL_ !ASTMetadata (Judgement a) !(Proof a)
  | IffElimR_ !ASTMetadata (Judgement a) !(Proof a)
  | IffIntr_ !ASTMetadata (Judgement a) !(Proof a) !(Proof a)

instance AST (Proof a) where
  type Root (Proof a) = Judgement a

  root (Axiom j) = j
  root (FalseElim j _) = j
  root (TrueIntr j) = j
  root (NotElim j _ _) = j
  root (NotIntr j _) = j
  root (ImpElim j _ _) = j
  root (ImpIntr j _) = j
  root (OrElim j _ _ _) = j
  root (OrIntrL j _) = j
  root (OrIntrR j _) = j
  root (AndElimL j _) = j
  root (AndElimR j _) = j
  root (AndIntr j _ _) = j
  root (IffElimL j _) = j
  root (IffElimR j _) = j
  root (IffIntr j _ _) = j

  children (Axiom _) = []
  children (FalseElim _ p) = [p]
  children (TrueIntr _) = []
  children (NotElim _ p q) = [p, q]
  children (NotIntr _ p) = [p]
  children (ImpElim _ p q) = [p, q]
  children (ImpIntr _ p) = [p]
  children (OrElim _ p q r) = [p, q, r]
  children (OrIntrL _ p) = [p]
  children (OrIntrR _ p) = [p]
  children (AndElimL _ p) = [p]
  children (AndElimR _ p) = [p]
  children (AndIntr _ p q) = [p, q]
  children (IffElimL _ p) = [p]
  children (IffElimR _ p) = [p]
  children (IffIntr _ p q) = [p, q]

  metadata (Axiom_ m _) = m
  metadata (FalseElim_ m _ _) = m
  metadata (TrueIntr_ m _) = m
  metadata (NotElim_ m _ _ _) = m
  metadata (NotIntr_ m _ _) = m
  metadata (ImpElim_ m _ _ _) = m
  metadata (ImpIntr_ m _ _) = m
  metadata (OrElim_ m _ _ _ _) = m
  metadata (OrIntrL_ m _ _) = m
  metadata (OrIntrR_ m _ _) = m
  metadata (AndElimL_ m _ _) = m
  metadata (AndElimR_ m _ _) = m
  metadata (AndIntr_ m _ _ _) = m
  metadata (IffElimL_ m _ _) = m
  metadata (IffElimR_ m _ _) = m
  metadata (IffIntr_ m _ _ _) = m

-- Smart constructors and pattern matching synonyms

-- | An axiom @('Axiom' (g 'AutoProof.Judgement.|-' a))@ represents the
-- inference of the judgement \(g \vdash a\), where \(a \in g\):
--
-- \[
--   \frac{}{
--     g \vdash a
--   }
--   \, (\text{Axiom})
-- \]
pattern Axiom :: Judgement a -> Proof a
pattern Axiom a <-
  Axiom_ _ a
  where
    Axiom = atomicASTConstructor Axiom_

-- | Falsity elimination (principle of explosion).
-- @('FalseElim' (g 'AutoProof.Judgement.|-' a) p)@ represents the inference of
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
pattern FalseElim :: Judgement a -> Proof a -> Proof a
pattern FalseElim j p <-
  FalseElim_ _ j p
  where
    FalseElim = unaryRootedASTConstructor FalseElim_

-- | Truth introduction.
-- @('TrueIntr' (g 'AutoProof.Judgement.|-' 'AutoProof.Formula.Lit' 'True'))@
-- represents the vacuous inference of the judgement \(g \vdash \top\):
--
-- \[
--   \frac{}{
--     g \vdash \top
--   }
--   \, (\top\text{I})
-- \]
pattern TrueIntr :: Judgement a -> Proof a
pattern TrueIntr j <-
  TrueIntr_ _ j
  where
    TrueIntr = atomicASTConstructor TrueIntr_

-- | Negation elimination.
-- @('NotElim' (g 'AutoProof.Judgement.|-' 'AutoProof.Formula.Lit' 'False') p q)@
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
pattern NotElim :: Judgement a -> Proof a -> Proof a -> Proof a
pattern NotElim j p q <-
  NotElim_ _ j p q
  where
    NotElim = binaryRootedASTConstructor NotElim_

-- | Negation introduction.
-- @('NotIntr' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.Not' a)) p)@
-- represents the inference of the judgement \(g \vdash \lnot a\) given a proof
-- \(p\) of \(g, a \vdash \bot\):
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
pattern NotIntr :: Judgement a -> Proof a -> Proof a
pattern NotIntr j p <-
  NotIntr_ _ j p
  where
    NotIntr = unaryRootedASTConstructor NotIntr_

-- | Implication elimination (/modus ponens/).
-- @('ImpElim' (g 'AutoProof.Judgement.|-' b) p q)@ represents the inference of
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
pattern ImpElim :: Judgement a -> Proof a -> Proof a -> Proof a
pattern ImpElim j p q <-
  ImpElim_ _ j p q
  where
    ImpElim = binaryRootedASTConstructor ImpElim_

-- | Implication introduction.
-- @('ImpIntr' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.Imp' a b) p)@
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
pattern ImpIntr :: Judgement a -> Proof a -> Proof a
pattern ImpIntr j p <-
  ImpIntr_ _ j p
  where
    ImpIntr = unaryRootedASTConstructor ImpIntr_

-- | Disjunction elimination.
-- @('OrElim' (g 'AutoProof.Judgement.|-' c) p q r)@ represents the inference of
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
pattern OrElim :: Judgement a -> Proof a -> Proof a -> Proof a -> Proof a
pattern OrElim j p q r <-
  OrElim_ _ j p q r
  where
    OrElim = ternaryRootedASTConstructor OrElim_

-- | Left disjunction introduction.
-- @('OrIntrL' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.Or' a b)) p)@
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
pattern OrIntrL :: Judgement a -> Proof a -> Proof a
pattern OrIntrL j p <-
  OrIntrL_ _ j p
  where
    OrIntrL = unaryRootedASTConstructor OrIntrL_

-- | Right disjunction introduction.
-- @('OrIntrR' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.Or' a b)) p)@
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
pattern OrIntrR :: Judgement a -> Proof a -> Proof a
pattern OrIntrR j p <-
  OrIntrR_ _ j p
  where
    OrIntrR = unaryRootedASTConstructor OrIntrR_

-- | Left conjunction elimination.
-- @('AndElimL' (g 'AutoProof.Judgement.|-' a) p)@ represents the inference of
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
pattern AndElimL :: Judgement a -> Proof a -> Proof a
pattern AndElimL j p <-
  AndElimL_ _ j p
  where
    AndElimL = unaryRootedASTConstructor AndElimL_

-- | Right conjunction elimination.
-- @('AndElimR' (g 'AutoProof.Judgement.|-' b) p)@ represents the inference of
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
pattern AndElimR :: Judgement a -> Proof a -> Proof a
pattern AndElimR j p <-
  AndElimR_ _ j p
  where
    AndElimR = unaryRootedASTConstructor AndElimR_

-- | Conjunction introduction.
-- @('AndIntr' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.And' a b)) p)@
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
pattern AndIntr :: Judgement a -> Proof a -> Proof a -> Proof a
pattern AndIntr j p q <-
  AndIntr_ _ j p q
  where
    AndIntr = binaryRootedASTConstructor AndIntr_

-- | Left equivalence elimination.
-- @('IffElimL' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.Imp' a b)) p)@
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
pattern IffElimL :: Judgement a -> Proof a -> Proof a
pattern IffElimL j p <-
  IffElimL_ _ j p
  where
    IffElimL = unaryRootedASTConstructor IffElimL_

-- | Right equivalence elimination.
-- @('IffElimR' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.Imp' b a)) p)@
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
pattern IffElimR :: Judgement a -> Proof a -> Proof a
pattern IffElimR j p <-
  IffElimR_ _ j p
  where
    IffElimR = unaryRootedASTConstructor IffElimR_

-- | Equivalence introduction.
-- @('IffIntr' (g 'AutoProof.Judgement.|-' ('AutoProof.Formula.Iff' a b)) p)@
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
pattern IffIntr :: Judgement a -> Proof a -> Proof a -> Proof a
pattern IffIntr j p q <-
  IffIntr_ _ j p q
  where
    IffIntr = binaryRootedASTConstructor IffIntr_

{-# COMPLETE
  Axiom,
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
  #-}

-- Instance declarations

instance Eq a => Eq (Proof a) where
  (Axiom j) == (Axiom j') = j == j'
  (FalseElim j p) == (FalseElim j' p') = j == j' && p == p'
  (TrueIntr j) == (TrueIntr j') = j == j'
  (NotElim j p q) == (NotElim j' p' q') = j == j' && p == p' && q == q'
  (NotIntr j p) == (NotIntr j' p') = j == j' && p == p'
  (ImpElim j p q) == (ImpElim j' p' q') = j == j' && p == p' && q == q'
  (ImpIntr j p) == (ImpIntr j' p') = j == j' && p == p'
  (OrElim j p q r) == (OrElim j' p' q' r') = j == j' && p == p' && q == q' && r == r'
  (OrIntrL j p) == (OrIntrL j' p') = j == j' && p == p'
  (OrIntrR j p) == (OrIntrR j' p') = j == j' && p == p'
  (AndElimL j p) == (AndElimL j' p') = j == j' && p == p'
  (AndElimR j p) == (AndElimR j' p') = j == j' && p == p'
  (AndIntr j p q) == (AndIntr j' p' q') = j == j' && p == p' && q == q'
  (IffElimL j p) == (IffElimL j' p') = j == j' && p == p'
  (IffElimR j p) == (IffElimR j' p') = j == j' && p == p'
  (IffIntr j p q) == (IffIntr j' p' q') = j == j' && p == p' && q == q'
  _ == _ = False

instance Ord a => Ord (Proof a) where
  compare p p' = case compare (height p) (height p') of
    EQ -> case p of
      Axiom j -> case p' of
        Axiom j' -> compare j j'
        _ -> LT
      FalseElim j q -> case p' of
        Axiom {} -> GT
        FalseElim j' q' -> compareUnary j q j' q'
        _ -> LT
      TrueIntr j -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr j' -> compare j j'
        _ -> LT
      NotElim j q r -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim j' q' r' -> compareBinary j q r j' q' r'
        _ -> LT
      NotIntr j q -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr j' q' -> compareUnary j q j' q'
        _ -> LT
      ImpElim j q r -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim j' q' r' -> compareBinary j q r j' q' r'
        _ -> LT
      ImpIntr j q -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr j' q' -> compareUnary j q j' q'
        _ -> LT
      OrElim j q r s -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim j' q' r' s' -> compareTernary j q r s j' q' r' s'
        _ -> LT
      OrIntrL j q -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL j' q' -> compareUnary j q j' q'
        _ -> LT
      OrIntrR j q -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR j' q' -> compareUnary j q j' q'
        _ -> LT
      AndElimL j q -> case p' of
        Axiom {} -> GT
        FalseElim {} -> GT
        TrueIntr {} -> GT
        NotElim {} -> GT
        NotIntr {} -> GT
        ImpElim {} -> GT
        ImpIntr {} -> GT
        OrElim {} -> GT
        OrIntrL {} -> GT
        OrIntrR {} -> GT
        AndElimL j' q' -> compareUnary j q j' q'
        _ -> LT
      AndElimR j q -> case p' of
        Axiom {} -> GT
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
        AndElimR j' q' -> compareUnary j q j' q'
        _ -> LT
      AndIntr j q r -> case p' of
        Axiom {} -> GT
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
        AndIntr j' q' r' -> compareBinary j q r j' q' r'
        _ -> LT
      IffElimL j q -> case p' of
        Axiom {} -> GT
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
        IffElimL j' q' -> compareUnary j q j' q'
        _ -> LT
      IffElimR j q -> case p' of
        Axiom {} -> GT
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
        IffElimR j' q' -> compareUnary j q j' q'
        _ -> LT
      IffIntr j q r -> case p' of
        Axiom {} -> GT
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
        IffIntr j' q' r' -> compareBinary j q r j' q' r'
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

      g (Axiom j) = showsNullary "Axiom " j
      g (FalseElim j p) = showsUnary "FalseElim " j p
      g (TrueIntr j) = showsNullary "TrueIntr " j
      g (NotElim j p q) = showsBinary "NotElim " j p q
      g (NotIntr j p) = showsUnary "NotIntr " j p
      g (ImpElim j p q) = showsBinary "ImpElim " j p q
      g (ImpIntr j p) = showsUnary "ImpIntr " j p
      g (OrElim j p q r) = showsTernary "OrElim " j p q r
      g (OrIntrL j p) = showsUnary "OrIntrL " j p
      g (OrIntrR j p) = showsUnary "OrIntrR " j p
      g (AndElimL j p) = showsUnary "AndElimL " j p
      g (AndElimR j p) = showsUnary "AndElimR " j p
      g (AndIntr j p q) = showsBinary "AndIntr " j p q
      g (IffElimL j p) = showsUnary "IffElimL " j p
      g (IffElimR j p) = showsUnary "IffElimR " j p
      g (IffIntr j p q) = showsBinary "IffIntr " j p q

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

instance (Ord a, Read a) => Read (Proof a) where
  readsPrec d = f (d > appPrec)
    where
      appPrec = 10

      f b s = concatMap (($ s) . readParen b) readers
      readers =
        [ readAxiom,
          readFalseElim,
          readTrueIntr,
          readNotElim,
          readNotIntr,
          readImpElim,
          readImpIntr,
          readOrElim,
          readOrIntrL,
          readOrIntrR,
          readAndElimL,
          readAndElimR,
          readAndIntr,
          readIffElimL,
          readIffElimR,
          readIffIntr
        ]
      readAxiom = parseNullary Axiom "Axiom"
      readFalseElim = parseUnary FalseElim "FalseElim"
      readTrueIntr = parseNullary TrueIntr "TrueIntr"
      readNotElim = parseBinary NotElim "NotElim"
      readNotIntr = parseUnary NotIntr "NotIntr"
      readImpElim = parseBinary ImpElim "ImpElim"
      readImpIntr = parseUnary ImpIntr "ImpIntr"
      readOrElim = parseTernary OrElim "OrElim"
      readOrIntrL = parseUnary OrIntrL "OrIntrL"
      readOrIntrR = parseUnary OrIntrR "OrIntrR"
      readAndElimL = parseUnary AndElimL "AndElimL"
      readAndElimR = parseUnary AndElimR "AndElimR"
      readAndIntr = parseBinary AndIntr "AndIntr"
      readIffElimL = parseUnary IffElimL "IffElimL"
      readIffElimR = parseUnary IffElimR "IffElimR"
      readIffIntr = parseBinary IffIntr "IffIntr"

      parseNullary c n s =
        [ (c j, u)
          | (n', t) <- lex s,
            n == n',
            (j, u) <- readsPrec (appPrec + 1) t
        ]
      parseUnary c n s =
        [ (c j p, v)
          | (n', t) <- lex s,
            n == n',
            (j, u) <- readsPrec (appPrec + 1) t,
            (p, v) <- f True u
        ]
      parseBinary c n s =
        [ (c j p q, w)
          | (n', t) <- lex s,
            n == n',
            (j, u) <- readsPrec (appPrec + 1) t,
            (p, v) <- f True u,
            (q, w) <- f True v
        ]
      parseTernary c n s =
        [ (c j p q r, x)
          | (n', t) <- lex s,
            n == n',
            (j, u) <- readsPrec (appPrec + 1) t,
            (p, v) <- f True u,
            (q, w) <- f True v,
            (r, x) <- f True w
        ]

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

      rule Axiom {} = axiomS
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
