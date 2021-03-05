{-# OPTIONS_HADDOCK hide, prune #-}

-- |
-- Module      : AutoProof.Utils.Parser.Char
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines character- and string-related parsers.
module AutoProof.Utils.Parser.Char
  ( charIf,
    char,
    anyChar,
    spaces,
    string,
    oneOf,
  )
where

import AutoProof.Utils.Parser.Types (Parser (Parser))
import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Char (isSpace)

charIf :: (Char -> Bool) -> Parser Char
charIf p = Parser f
  where
    f (c : cs) | p c = Just (cs, c)
    f _ = Nothing

char :: Char -> Parser Char
char c = charIf (== c)

anyChar :: Parser Char
anyChar = charIf (const True)

oneOf :: [Char] -> Parser Char
oneOf = foldl (<|>) empty . map char

string :: String -> Parser String
string = traverse char

spaces :: Parser String
spaces = many (charIf isSpace)
