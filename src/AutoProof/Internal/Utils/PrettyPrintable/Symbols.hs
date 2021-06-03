{-# LANGUAGE CPP #-}

-- |
-- Module      : AutoProof.Internal.Utils.PrettyPrintable.Symbols
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines string constants ("symbols") used for pretty-printing.
module AutoProof.Internal.Utils.PrettyPrintable.Symbols
  ( -- * Formula symbols
    falseS,
    trueS,
    notS,
    impS,
    orS,
    andS,
    iffS,

    -- * Judgement symbols
    turnstileS,

    -- * Proof symbols
    axiomS,
    falseElimS,
    trueIntrS,
    notElimS,
    notIntrS,
    impElimS,
    impIntrS,
    orElimS,
    orIntrLS,
    orIntrRS,
    andElimLS,
    andElimRS,
    andIntrS,
    iffElimLS,
    iffElimRS,
    iffIntrS,

    -- * Proof pretty-printing
    vertS,
    cornerS,
    branchS,
  )
where

-- Propositional literals, connectives, and judgement turnstile

-- | Falsity symbol.
falseS :: String
falseS = ifWindows "false" "⊥"

-- | Truth symbol.
trueS :: String
trueS = ifWindows "true" "⊤"

-- | Negation symbol.
notS :: String
notS = ifWindows "~" "¬"

-- | Implication symbol.
impS :: String
impS = ifWindows "->" "→"

-- | Disjunction symbol.
orS :: String
orS = ifWindows "|" "∨"

-- | Conjunction symbol.
andS :: String
andS = ifWindows "&" "∧"

-- | Equivalence symbol.
iffS :: String
iffS = ifWindows "<->" "↔"

-- | Judgement turnstile symbol.
turnstileS :: String
turnstileS = ifWindows "|-" "⊢"

-- Inference rule symbols

-- | Axiom symbol.
axiomS :: String
axiomS = "(Ax)"

-- | Falsity elimination symbol.
falseElimS :: String
falseElimS = elim falseS

-- | Truth introduction symbol.
trueIntrS :: String
trueIntrS = intr trueS

-- | Negation elimination symbol.
notElimS :: String
notElimS = elim notS

-- | Negation introduction symbol.
notIntrS :: String
notIntrS = intr notS

-- | Implication elimination symbol.
impElimS :: String
impElimS = elim impS

-- | Implication introduction symbol.
impIntrS :: String
impIntrS = intr impS

-- | Disjunction elimination symbol.
orElimS :: String
orElimS = elim orS

-- | Disjunction introduction (left) symbol.
orIntrLS :: String
orIntrLS = intrL orS

-- | Disjunction introduction (right) symbol.
orIntrRS :: String
orIntrRS = intrR orS

-- | Conjunction elimination (left) symbol.
andElimLS :: String
andElimLS = elimL andS

-- | Conjunction elimination (right) symbol.
andElimRS :: String
andElimRS = elimR andS

-- | Conjunction introduction symbol.
andIntrS :: String
andIntrS = intr andS

-- | Equivalence elimination (left) symbol.
iffElimLS :: String
iffElimLS = elimL iffS

-- | Equivalence elimination (right) symbol.
iffElimRS :: String
iffElimRS = elimR iffS

-- | Equivalence introduction symbol.
iffIntrS :: String
iffIntrS = intr iffS

-- Inference rule symbol helper functions

elim :: String -> String
elim s = "(" ++ s ++ "E)"

elimL :: String -> String
elimL s = "(" ++ s ++ "EL)"

elimR :: String -> String
elimR s = "(" ++ s ++ "ER)"

intr :: String -> String
intr s = "(" ++ s ++ "I)"

intrL :: String -> String
intrL s = "(" ++ s ++ "IL)"

intrR :: String -> String
intrR s = "(" ++ s ++ "IR)"

-- Pretty-printed proof components

-- | Vertical bar symbol used in pretty-printing proofs.
vertS :: String
vertS = ifWindows "|" "│"

-- | Corner symbol used in pretty-printing proofs.
cornerS :: String
cornerS = ifWindows "+-- " "┌── "

-- | Branch symbol used in pretty-printing proofs.
branchS :: String
branchS = ifWindows "+-- " "├── "

-- Use first choice if Windows, use second choice if other OS
-- TODO: figure out if Windows is actually the problem with printing certain
-- characters
ifWindows :: a -> a -> a

#ifdef mingw32_HOST_OS
ifWindows a _ = a
#else
ifWindows _ a = a
#endif
