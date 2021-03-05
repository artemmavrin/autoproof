module Main where

import AutoProof
  ( parseFormula,
    prettyFormula,
    prettyProof,
    correct,
    proveTautology,
    (|-)
  )
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "proveTautology REPL"
  loop
  where
    loop = do
      putStr "(Ctrl+C to quit)> "
      line <- getLine
      if null line
        then loop
        else do
          case parseFormula line of
            Left e -> print e
            Right a -> do
              putStrLn $ "Trying to prove " ++ prettyFormula a
              case proveTautology a of
                Nothing -> putStrLn "No proof found"
                Just p -> do
                  putStrLn "Found proof:"
                  putStrLn $ prettyProof p
                  if correct ([] |- a) p
                    then putStrLn "The proof is correct"
                    else putStrLn "The proof is incorrect"
          loop
