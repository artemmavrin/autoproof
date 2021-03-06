module Main where

import AutoProof
  ( correct,
    debug,
    parseFormula,
    prettyFormula,
    prettyProof,
    proveTautology,
    (|-),
  )
import AutoProof.Utils.Symbols (turnstileS)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "proveTautology REPL"
  loop
  where
    loop = do
      putStr $ "(Ctrl+C to quit) " ++ turnstileS ++ " "
      line <- getLine
      if null line
        then loop
        else do
          case parseFormula line of
            Left e -> print e
            Right a -> do
              putStrLn $ "Trying to prove " ++ turnstileS ++ " " ++ prettyFormula a
              case proveTautology a of
                Nothing -> putStrLn "No proof found"
                Just p -> do
                  putStrLn "Found proof:"
                  putStrLn $ prettyProof p
                  if correct ([] |- a) p
                    then putStrLn "The proof is correct"
                    else do
                      putStrLn "The proof is incorrect"
                      case debug p of
                        Right () -> return ()
                        Left q -> do
                          putStrLn "Invalid inference:"
                          putStrLn $ prettyProof q
          loop
