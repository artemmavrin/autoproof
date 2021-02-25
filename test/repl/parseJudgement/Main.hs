module Main where

import AutoProof (parseJudgement, prettyJudgement)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "parseJudgement REPL"
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
            Right p -> do
              putStrLn $ prettyJudgement p
              print p
          loop
