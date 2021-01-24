{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Data.Prop.Parser
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Implements a parser for propositional logic formulas
module Data.Prop.Parser
  ( -- * Safe parsing
    parseFormula,
    parseSequent,

    -- * Unsafe parsing
    unsafeParseFormula,
    unsafeParseSequent,
  )
where

import Data.Functor.Identity (Identity)
import Data.Prop.Types (Context, Formula (And, Lit, Imp, Not, Or, Var), Sequent)
import qualified Data.Set as Set (fromList)
import Text.Parsec.Char (char, oneOf, spaces, string)
import Text.Parsec.Combinator
  ( between,
    chainl1,
    chainr1,
    eof,
    notFollowedBy,
    sepBy,
  )
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (ParsecT, Stream, many, parse, parserZero, try, (<|>))
import Prelude hiding (and, or, not)

-- | @(parseFormula s)@ parses a string-like value @s@ as a propositional
-- formula, returning a @'Right' a@ on success, where @a@ is the parsed term.
-- Otherwise, @'Left' e@ is returned, where @e@ is a 'ParseError'.
--
-- __Conventions__
--
-- * Supported connective symbols:
--
--     * Negation: @"~"@, @"¬"@.
--     * Implication: @"->"@, @"=>"@, @"→"@, @"⇾"@, @"⇒"@ @"⊃"@.
--     * Disjunction: @"|"@, @"\\/"@, @"∨"@
--     * Conjunction: @"&"@, @"/\\"@, @"∧"@
--
-- * Implication is right-associative and has lower precedence than the other
--   connectives.
-- * Conjunction and disjunction have the same precedence, and expressions
--   involving both are evaluated left-to-right.
-- * Negation binds most tightly, and must immediately precede its argument
--   (i.e., there should not be a space between a negation symbol and the
--   proposition that follows).
-- * Valid variable names begin with a letter (uppercase or lowercase) or an
--   underscore, and may be followed by a letter, underscore, digit, or single
--   quote (a "prime" symbol). The exceptions are the strings @"false"@ and
--   @"true"@, which are parsed as the literals \(\bot\) and \(\top\),
--   respectively.
--
-- ==== __Examples__
--
-- >>> parseFormula "a -> b -> c"
-- Right (Imp (Var "a") (Imp (Var "b") (Var "c")))
--
-- >>> parseFormula "~a | b -> c"
-- Right (Imp (Or (Not (Var "a")) (Var "b")) (Var "c"))
--
-- >>> parseFormula "(a -> b) & ~c"
-- Right (And (Imp (Var "a") (Var "b")) (Not (Var "c")))
parseFormula :: Stream s Identity Char => s -> Either ParseError (Formula String)
parseFormula = parse (formula <* eof) ""

-- | @(parseSequent s)@ parses a string-like value @s@ as a propositional
-- sequent. The context is a (potentially empty) comma-separated list of
-- formulas, the judgement turnstile symbol is either @"|-"@ or @"⊢"@, and the
-- conclusion is another formula.
--
-- ==== __Examples__
--
-- >>> parseSequent "a, a -> b |- b"
-- Right (fromList [Var "a",Imp (Var "a") (Var "b")],Var "b")
--
-- >>> parseSequent "a |- a | b"
-- Right (fromList [Var "a"],Or (Var "a") (Var "b"))
parseSequent :: Stream s Identity Char => s -> Either ParseError (Sequent String)
parseSequent = parse (sequent <* eof) ""

-- | @(unsafeParseFormula s)@ parses a string-like value @s@ as a propositional
-- formula, returning the parsed formula on success, and throwing an error on
-- failure.
--
-- ==== __Examples__
--
-- >>> unsafeParseFormula "(a => b) => c"
-- Imp (Imp (Var "a") (Var "b")) (Var "c")
unsafeParseFormula :: Stream s Identity Char => s -> Formula String
unsafeParseFormula = either (error . show) id . parseFormula

-- | @(unsafeParseSequent s)@ parses a string-like value @s@ as a propositional
-- sequent, returning the parsed sequent on success, and throwing an error on
-- failure.
--
-- ==== __Examples__
--
-- >>> unsafeParseSequent "a, b |- a -> b"
-- (fromList [Var "a",Var "b"],Imp (Var "a") (Var "b"))
--
-- >>> unsafeParseSequent "a & b |- a"
-- (fromList [And (Var "a") (Var "b")],Var "a")
unsafeParseSequent :: Stream s Identity Char => s -> Sequent String
unsafeParseSequent = either (error . show) id . parseSequent

-- Top-level parsers

formula :: Stream s m Char => ParsecT s u m (Formula String)
formula = padded implies

context :: Stream s m Char => ParsecT s u m (Context String)
context = Set.fromList <$> formula `sepBy` char ','

sequent :: Stream s m Char => ParsecT s u m (Sequent String)
sequent = (,) <$> context <*> (spaces *> turnstile *> formula)

-- Connective symbols

implication :: Stream s m Char => ParsecT s u m String
implication = connective ["->", "=>", "→", "⇾", "⇒", "⊃"]

and :: Stream s m Char => ParsecT s u m String
and = connective ["&", "∧", "/\\"]

or :: Stream s m Char => ParsecT s u m String
or = connective ["|", "∨", "\\/"]

negation :: Stream s m Char => ParsecT s u m String
negation = connective ["~", "¬"]

turnstile :: Stream s m Char => ParsecT s u m String
turnstile = connective ["|-", "⊢"]

-- Propositional formula components

-- Right-associative nested implications
implies :: Stream s m Char => ParsecT s u m (Formula String)
implies = chainr1 andOr (try $ padded (Imp <$ implication))

-- Left-associative nested disjunctions and conjunctions
andOr :: Stream s m Char => ParsecT s u m (Formula String)
andOr = chainl1 not (try $ padded $ (And <$ and) <|> (Or <$ or))

-- Zero or more consecutive negations
not :: Stream s m Char => ParsecT s u m (Formula String)
not = spaces *> (foldl (.) id <$> try (many (Not <$ negation)) <*> atom)

atom :: Stream s m Char => ParsecT s u m (Formula String)
atom = enclosed formula <|> try false <|> try true <|> variable

false :: Stream s m Char => ParsecT s u m (Formula String)
false = Lit False <$ (string "⊥" <|> string "false")

true :: Stream s m Char => ParsecT s u m (Formula String)
true = Lit True <$ (string "⊤" <|> string "true")

variable :: Stream s m Char => ParsecT s u m (Formula String)
variable = Var <$> name
  where
    name = (:) <$> nameStart <*> many nameChar
    nameStart = oneOf $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
    nameChar = nameStart <|> oneOf ('\'' : ['0' .. '9'])

-- Helper functions

-- Ignore space on either side of a parsed string.
padded :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
padded = between spaces spaces

-- Parse a string surrounded by parentheses.
enclosed :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
enclosed = between (char '(') (char ')')

-- Generate a parser out of a collection of strings. The parser will parse any
-- one of the given strings.
connective :: Stream s m Char => [String] -> ParsecT s u m String
connective [] = parserZero
connective (c : cs)
  -- Special case needed to handle the turnstile symbol "|-" separately from the
  -- disjunction symbol "|".
  | c == "|" = try (string "|" <* notFollowedBy (char '-')) <|> connective cs
  | otherwise = try (string c) <|> connective cs
