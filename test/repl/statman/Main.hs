module Main where

import AutoProof
  ( parseFormula,
    prettyFormula,
    prettyProof,
    proveImp,
    strengthenProof,
  )
import AutoProof.Proof.Provability (toImp)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "proveImp REPL"
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
            Right f -> do
              putStrLn $ "Trying to prove " ++ prettyFormula f
              case proveImp (toImp f) of
                Nothing -> putStrLn "No proof found"
                Just p -> do
                  putStrLn "Found proof:"
                  putStrLn $ prettyProof (strengthenProof p)
          loop
