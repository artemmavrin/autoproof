-- |
-- Module      : Data.Prop.Proof.Types
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Natural deduction proof trees for intuitionistic propositional logic.
module Data.Prop.Proof.Types
  ( Proof
      ( Ax,
        TopIntr,
        BotElim,
        NotElim,
        NotIntr,
        ImpElim,
        ImpIntr,
        OrElim,
        OrIntrL,
        OrIntrR,
        AndElimL,
        AndElimR,
        AndIntr
      ),
    prettyProof,
  )
where

import Data.List (intercalate)
import Data.Prop.Types (Context, Formula (..))
import Data.Prop.Utils (PrettyPrintable (pretty))

-- | A natural deduction proof tree for intuitionistic propositional logic.
data Proof a
  = -- | An axiom @('Ax' g a)@ represents the inference of the sequent
    -- \(g \vdash a\), where the propositional formula \(a\) belongs to the
    -- context \(g\).
    Ax (Context a) (Formula a)
  | -- | Truth introduction. @('TopIntr' g ('Lit' 'True'))@ represents an
    -- inference of the sequent \(g \vdash \top\).
    TopIntr (Context a) (Formula a)
  | -- | Falsity elimination (principle of explosion). @('BotElim' g a p)@
    -- represents the inference \(g \vdash a\) from a proof \(p\) of
    -- \(g \vdash \bot\).
    BotElim (Context a) (Formula a) (Proof a)
  | -- | Negation elimination. @('NotElim' g ('Lit' 'False') p q)@ represents
    -- the inference of the sequent \(g \vdash \bot\) given a proof \(p\) of
    -- \(g \vdash \lnot a\) and a proof \(q\) of \(g \vdash a\).
    NotElim (Context a) (Formula a) (Proof a) (Proof a)
  | -- | Negation introduction. @('NotIntr' g ('Not' a) p)@ represents the
    -- inference of the sequent \(g \vdash \neg a\) from a proof \(p\) of
    -- \(g, a \vdash \bot\).
    NotIntr (Context a) (Formula a) (Proof a)
  | -- | Implication elimination (modus ponens). @('ImpElim' g b p q)@
    -- represents the inference of the sequent \(g \vdash b\) given a proof
    -- \(p\) of \(g \vdash a \rightarrow b\) and a proof \(q\) of
    -- \(g \vdash q\).
    ImpElim (Context a) (Formula a) (Proof a) (Proof a)
  | -- | Implication introduction. @('ImpIntr' g ('Imp' a b) p)@ represents the
    -- inference of the sequent \(g \vdash a \rightarrow b\) given a proof \(p\)
    -- of \(g, a \vdash b\).
    ImpIntr (Context a) (Formula a) (Proof a)
  | -- | Disjunction elimination. @('OrElim' g c p q r)@ represents an inference
    -- of the sequent \(g \vdash c\) given a proof \(p\) of
    -- \(g \vdash a \lor b\), a proof \(q\) of \(g, a \vdash c\), and a proof
    -- \(r\) of \(g, b \vdash c\).
    OrElim (Context a) (Formula a) (Proof a) (Proof a) (Proof a)
  | -- | Disjunction introduction (left). @('OrIntrL' g ('Or' a b) p)@
    -- represents an inference of the sequent \(g \vdash a \lor b\) given a
    -- proof \(p\) of \(g \vdash a\).
    OrIntrL (Context a) (Formula a) (Proof a)
  | -- | Disjunction introduction (right). @('OrIntrR' g ('Or' a b) p)@
    -- represents an inference of the sequent \(g \vdash a \lor b\) given a
    -- proof \(p\) of \(g \vdash b\).
    OrIntrR (Context a) (Formula a) (Proof a)
  | -- | Conjunction elimination (left). @('AndElimL` g a p)@ represents the
    -- inference of the sequent \(g \vdash a\) given a proof \(p\) of
    -- \(g \vdash a \land b\).
    AndElimL (Context a) (Formula a) (Proof a)
  | -- | Conjunction elimination (right). @('AndElimR` g b p)@ represents the
    -- inference of the sequent \(g \vdash b\) given a proof \(p\) of
    -- \(g \vdash a \land b\).
    AndElimR (Context a) (Formula a) (Proof a)
  | -- | Conjunction introduction. @('AndIntr' g ('And' a b) p q)@ represents
    -- the inference of the sequent \(g \vdash a \land b\) given a proof \(p\)
    -- of \(g \vdash a\) and a proof \(q\) of \(g \vdash b\).
    AndIntr (Context a) (Formula a) (Proof a) (Proof a)
  deriving (Eq, Show)

-- | TODO
prettyProof :: PrettyPrintable a => Proof a -> String
prettyProof = pp 0
  where
    pp n (Ax c a) = concatIndent n [pretty (c, a), "    (Ax)"]
    pp n (ImpIntr c a p) = concatIndent n [pretty (c, a), "    (->I)", pp (n + 4) p]
    pp n (ImpElim c a p q) = concatIndent n [pretty (c, a), "    (->E)", pp (n + 4) p, pp (n + 4) q]
    pp _ _ = undefined

    concatIndent :: Int -> [String] -> String
    concatIndent n l = intercalate "\n" $ (indent n ++) <$> l

    indent :: Int -> String
    indent 0 = ""
    indent n = ' ' : indent (n - 1)

instance PrettyPrintable a => PrettyPrintable (Proof a) where
  pretty = prettyProof
