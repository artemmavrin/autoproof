{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module AutoProof.Utils.Symbols
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
    andIntr,
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

falseS :: String
falseS = ifWindows "false" "⊥"

trueS :: String
trueS = ifWindows "true" "⊤"

notS :: String
notS = ifWindows "~" "¬"

impS :: String
impS = ifWindows "->" "→"

orS :: String
orS = ifWindows "|" "∨"

andS :: String
andS = ifWindows "&" "∧"

iffS :: String
iffS = ifWindows "<->" "↔"

turnstileS :: String
turnstileS = ifWindows "|-" "⊢"

-- Inference rule symbols

axiomS :: String
axiomS = "(Ax)"

falseElimS :: String
falseElimS = elim falseS

trueIntrS :: String
trueIntrS = intr trueS

notElimS :: String
notElimS = elim notS

notIntrS :: String
notIntrS = intr notS

impElimS :: String
impElimS = elim impS

impIntrS :: String
impIntrS = intr impS

orElimS :: String
orElimS = elim orS

orIntrLS :: String
orIntrLS = intrL orS

orIntrRS :: String
orIntrRS = intrR orS

andElimLS :: String
andElimLS = elimL andS

andElimRS :: String
andElimRS = elimR andS

andIntr :: String
andIntr = intr andS

iffElimLS :: String
iffElimLS = elimL iffS

iffElimRS :: String
iffElimRS = elimR iffS

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

vertS :: String
vertS = ifWindows "|" "│"

cornerS :: String
cornerS = ifWindows "+-- " "┌── "

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
