module Main where

import AutoProof
  ( Judgement (antecedents),
    parseJudgement,
    prettyJudgement,
    prettyProof,
    proveImp,
    strengthenProof,
    weakenProof,
  )
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
          case parseJudgement line of
            Left e -> print e
            Right j -> do
              putStrLn $ "Trying to prove " ++ prettyJudgement j
              case proveImp j of
                Nothing -> putStrLn "No proof found"
                Just p -> do
                  putStrLn "Found proof:"
                  let p' = strengthenProof p
                  let p'' = foldl weakenProof p' (antecedents j)
                  putStrLn $ prettyProof p''
          loop
