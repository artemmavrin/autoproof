{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module AutoProof.Utils.Symbols where

-- Propositional literals, connectives, and judgement turnstile
trueS, falseS, notS, impS, iffS, orS, andS, turnstileS :: String

#ifdef mingw32_HOST_OS
falseS = "false"
trueS = "true"
notS = "~"
impS = "->"
orS = "|"
andS = "&"
iffS = "<->"
turnstileS = "|-"
#else
falseS = "⊥"
trueS = "⊤"
notS = "¬"
impS = "→"
orS = "∨"
andS = "∧"
iffS = "↔"
turnstileS = "⊢"
#endif

-- Inference rule symbols
axiomS, impElimS, impIntrS :: String

axiomS = "(Ax)"
impElimS = "(" ++ impS ++ "E)"
impIntrS = "(" ++ impS ++ "I)"

-- Pretty-printed proof lines
vertS, cornerS, branchS :: String

#ifdef mingw32_HOST_OS
vertS = "|"
cornerS = "+-- "
branchS = "+-- "
#else
vertS = "│"
cornerS = "┌── "
branchS = "├── "
#endif
