-- Simple REPL for manually testing propositional formula parsing

module Main where

import Data.Prop
import Data.Prop.Utils (pretty)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
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
              putStrLn $ pretty p
              print p
          loop
