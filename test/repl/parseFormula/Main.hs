module Main where

import AutoProof (parseFormula, prettyFormula)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "parseFormula REPL"
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
            Right p -> do
              putStrLn $ prettyFormula p
              print p
          loop
