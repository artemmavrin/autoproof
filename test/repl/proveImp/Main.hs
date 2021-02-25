module Main where

import AutoProof
  ( parseJudgement,
    prettyJudgement,
    prettyProof,
    proveImp,
    strengthenProof,
  )
import Control.Monad (when)
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
                  putStrLn $ prettyProof p
                  let p' = strengthenProof p
                  when (p' /= p) $ do
                    putStrLn "\nFound proof with fewer hypotheses:"
                    putStrLn $ prettyProof p'
          loop
