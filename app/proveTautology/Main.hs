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
    findCut,
    parseFormula,
    pretty,
    proveTautology,
    (|-),
  )
import AutoProof.Utils.Symbols (turnstileS)
import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.Foldable (forM_)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO
  ( BufferMode (NoBuffering),
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
  )

data Opts = Opts
  { showCuts :: Bool,
    unknownOpts :: [String]
  }

header :: String
header = "proveTautology REPL"

prompt :: String
prompt = turnstileS ++ " "

findCutsArg :: String
findCutsArg = "--find-cuts"

allowedArgs :: [String]
allowedArgs = [findCutsArg]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  opts <- getOpts
  handleOpts opts
  putStrLn header
  loop opts

getOpts :: IO Opts
getOpts = do
  args <- getArgs
  return
    Opts
      { showCuts = findCutsArg `elem` args,
        unknownOpts = filter (`notElem` allowedArgs) args
      }

handleOpts :: Opts -> IO ()
handleOpts opts = do
  unless (null $ unknownOpts opts) (handleUnknownOpts opts)

handleUnknownOpts :: Opts -> IO ()
handleUnknownOpts opts = do
  hPutStrLn stderr ("Unknown options: " ++ intercalate ", " (unknownOpts opts))

loop :: Opts -> IO ()
loop opts = do
  putStr prompt
  getLine >>= handleLine opts . stripSpace
  loop opts

handleLine :: Opts -> String -> IO ()
handleLine _ "" = return ()
handleLine opts line = case parseFormula line of
  Nothing -> hPutStrLn stderr "Parse error"
  Just a -> handleFormula opts a

handleFormula :: Opts -> Formula String -> IO ()
handleFormula opts a = do
  putStrLn $ "Trying to prove " ++ pretty a
  case proveTautology a of
    Nothing -> putStrLn "No proof found"
    Just p -> handleProof opts a p

handleProof :: Opts -> Formula String -> Proof String -> IO ()
handleProof opts a p = do
  putStrLn "Found proof:"
  putStrLn $ pretty p
  when (showCuts opts) (forM_ (findCut p) (handleCut opts))
  unless (correct ([] |- a) p) (handleIncorrectProof opts p)

handleIncorrectProof :: Opts -> Proof String -> IO ()
handleIncorrectProof _ p = do
  hPutStrLn stderr "The proof is incorrect."
  case debug p of
    Right () -> return ()
    Left q -> do
      hPutStrLn stderr "Invalid inference:"
      hPutStrLn stderr $ pretty q

handleCut :: Opts -> Proof String -> IO ()
handleCut _ p = do
  hPutStrLn stderr "Found a cut in the proof:"
  hPutStrLn stderr $ pretty p

-- Remove whitespace at the beginning and end of a string
stripSpace :: String -> String
stripSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
