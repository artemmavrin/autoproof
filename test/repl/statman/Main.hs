module Main where

import AutoProof
  ( parseFormula,
    prettyFormula,
    prettyProof,
    proveImp,
    correct,
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
            Right a -> do
              putStrLn $ "Trying to prove " ++ prettyFormula a
              let j = toImp a
              case proveImp (toImp a) of
                Nothing -> putStrLn "No proof found"
                Just p -> do
                  putStrLn "Found proof:"
                  let p' = strengthenProof p
                  putStrLn $ prettyProof p'
                  if correct j p'
                    then putStrLn "The proof is correct"
                    else putStrLn "The proof is incorrect"
          loop
