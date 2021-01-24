-- Simple REPL for manually testing proof search in the implicational fragment
-- of intuitionistic propositional logic.

module Main where

import Data.List (intercalate)
import Data.Prop
import Data.Prop.Proof (proveImp)
import Data.Prop.Utils (PrettyPrintable (pretty))
import qualified Data.Set as Set
import System.IO
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

prettySequent :: PrettyPrintable a => Context a -> Formula a -> String
prettySequent c a = case Set.toList c of
  [] -> "|- " ++ pretty a
  c' -> intercalate ", " (map pretty c') ++ " |- " ++ pretty a
