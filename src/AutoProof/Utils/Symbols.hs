{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module AutoProof.Utils.Symbols where

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
