-- Simple REPL for trying out the proveTautology function.
--
-- At the prompt, enter any propositional logic formula (e.g., "a & b -> b"),
-- subject to the grammar specification of 'AutoProof.Parser.parseFormula'.

module Main where

import AutoProof
  ( Formula,
    Proof,
    correct,
    debug,
    parseFormula,
    pretty,
    proveTautology,
    (|-),
  )
import AutoProof.Utils.Symbols (turnstileS)
import Control.Monad (unless)
import Data.Char (isSpace)
import System.IO
  ( BufferMode (NoBuffering),
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
  )

header :: String
header = "proveTautology REPL"

prompt :: String
prompt = turnstileS ++ " "

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn header
  loop

loop :: IO ()
loop = do
  putStr prompt
  line <- getLine
  handleLine $ stripSpace line
  loop

handleLine :: String -> IO ()
handleLine "" = return ()
handleLine line = case parseFormula line of
  Nothing -> hPutStrLn stderr "Parse error"
  Just a -> handleFormula a

handleFormula :: Formula String -> IO ()
handleFormula a = do
  putStrLn $ "Trying to prove " ++ pretty a
  case proveTautology a of
    Nothing -> putStrLn "No proof found"
    Just p -> handleProof a p

handleProof :: Formula String -> Proof String -> IO ()
handleProof a p = do
  putStrLn "Found proof:"
  putStrLn $ pretty p
  unless (correct ([] |- a) p) (handleIncorrectProof p)

handleIncorrectProof :: Proof String -> IO ()
handleIncorrectProof p = do
  hPutStrLn stderr "The proof is incorrect."
  case debug p of
    Right () -> return ()
    Left q -> do
      hPutStrLn stderr "Invalid inference:"
      hPutStrLn stderr $ pretty q

-- Remove whitespace at the beginning and end of a string
stripSpace :: String -> String
stripSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
