-- Simple REPL for trying out the proveTautology function.
--
-- At the prompt, enter any propositional logic formula (e.g., "a & b -> b"),
-- subject to the grammar specification of 'AutoProof.Parser.parseFormula'.

module Main where

import AutoProof
  ( Judgement,
    Proof,
    correct,
    debug,
    findCut,
    parseJudgement,
    parseFormula,
    pretty,
    prove,
    (|-),
  )
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
    verbose :: Bool,
    unknownOpts :: [String]
  }

header :: String
header = "autoproof REPL"

prompt :: String
prompt = ">>>> "

findCutsArg :: String
findCutsArg = "--find-cuts"

verboseArg :: String
verboseArg = "--verbose"

allowedArgs :: [String]
allowedArgs = [findCutsArg, verboseArg]

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
        verbose = verboseArg `elem` args,
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
  Just a -> handleJudgement opts ([] |- a)
  Nothing -> case parseJudgement line of
    Just j -> handleJudgement opts j
    Nothing -> hPutStrLn stderr "Parse error"

handleJudgement :: Opts -> Judgement String -> IO ()
handleJudgement opts j = do
  when (verbose opts) (putStrLn $ "Trying to prove " ++ pretty j)
  case prove j of
    Nothing -> hPutStrLn stderr ("No proof found for " ++ pretty j)
    Just p -> handleProof opts j p

handleProof :: Opts -> Judgement String -> Proof String -> IO ()
handleProof opts j p = do
  when (verbose opts) (putStrLn "Found proof:")
  putStrLn $ pretty p
  if correct j p
    then do
      when (showCuts opts) (forM_ (findCut p) (handleCut opts))
    else do
      handleIncorrectProof opts p

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
