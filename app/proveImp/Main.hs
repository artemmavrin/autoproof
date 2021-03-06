-- Simple REPL for trying out the proveImp function.
--
-- At the prompt, enter any judgement in the implicational fragment of
-- propositional logic (e.g., "a, a -> b |- b"), subject to the grammar
-- specification of 'AutoProof.Parser.parseFormula' and
-- 'AutoProof.Parser.parseJudgement'.

module Main where

import AutoProof
  ( Judgement,
    Proof,
    correct,
    debug,
    parseJudgement,
    pretty,
    proveImp,
  )
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
header = "proveImp REPL"

prompt :: String
prompt = ">>> "

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
handleLine line = case parseJudgement line of
  Nothing -> hPutStrLn stderr "Parse error"
  Just j -> handleJudgement j

handleJudgement :: Judgement String -> IO ()
handleJudgement j = do
  putStrLn $ "Trying to prove " ++ pretty j
  case proveImp j of
    Nothing -> putStrLn "No proof found"
    Just p -> handleProof j p

handleProof :: Judgement String -> Proof String -> IO ()
handleProof j p = do
  putStrLn "Found proof:"
  putStrLn $ pretty p
  unless (correct j p) (handleIncorrectProof p)

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
