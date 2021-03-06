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

-- | Parse a single character if it satisfies a predicate
--
-- ==== __Examples__
--
-- >>> runParser (charIf isDigit) "123"
-- Just ("23",'1')
--
-- >>> runParser (charIf isDigit) "abc"
-- Nothing
charIf :: (Char -> Bool) -> Parser Char
charIf p = Parser f
  where
    f (c : cs) | p c = Just (cs, c)
    f _ = Nothing

-- | Parse a specific character.
--
-- ==== __Examples__
--
-- >>> runParser (char 'a') "abc"
-- Just ("bc",'a')
--
-- >>> runParser (char 'a') "def"
-- Nothing
char :: Char -> Parser Char
char c = charIf (== c)

-- | Parse any character.
--
-- ==== __Examples__
--
-- >>> runParser anyChar "abc"
-- Just ("bc",'a')
--
-- >>> runParser anyChar ""
-- Nothing
anyChar :: Parser Char
anyChar = charIf (const True)

-- | Parse one out of a list of characters.
--
-- ==== __Examples__
--
-- >>> runParser (oneOf ['a', 'b']) "bar"
-- Just ("ar",'b')
--
-- >>> runParser (oneOf ['a', 'b']) "foo"
-- Nothing
oneOf :: [Char] -> Parser Char
oneOf = foldr ((<|>) . char) empty

-- | Parse a specific string.
--
-- ==== __Examples__
--
-- >>> runParser (string "foo") "foobar"
-- Just ("bar","foo")
--
-- >>> runParser (string "foo") "fobar"
-- Nothing
string :: String -> Parser String
string = traverse char

-- | Parse zero or more spaces.
--
-- ==== __Examples__
--
-- >>> runParser spaces "   123"
-- Just ("123","   ")
--
-- >>> runParser spaces "123"
-- Just ("123","")
--
-- >>> runParser spaces ""
-- Just ("","")
spaces :: Parser String
spaces = many (charIf isSpace)
