{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module AutoProof.Utils.Symbols where

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

impElimS :: String
impElimS = elim impS

impIntrS :: String
impIntrS = intr impS

elim :: String -> String
elim s = "(" ++ s ++ "E)"

intr :: String -> String
intr s = "(" ++ s ++ "I)"

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
