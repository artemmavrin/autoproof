-- Simple REPL for manually testing proof search in the implicational fragment
-- of intuitionistic propositional logic.

module Main where

import Data.Prop (parseSequent, prettySequent)
import Data.Prop.Proof (proveImp)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Pretty.Simple (pPrint)

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
          case parseSequent line of
            Left e -> print e
            Right (c, a) -> do
              putStrLn $ "Trying to prove " ++ prettySequent c a
              case proveImp c a of
                Nothing -> putStrLn "No proof found"
                Just p -> pPrint p
          loop
