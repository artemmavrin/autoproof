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
import Data.Prop.Types (Context, Formula (And, Imp, Not, Or, Var), Sequent)
import qualified Data.Set as Set (fromList)
import Text.Parsec.Char (char, oneOf, spaces, string)
import Text.Parsec.Combinator
  ( between,
    chainl1,
    eof,
    notFollowedBy,
    sepBy,
    sepBy1,
  )
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (ParsecT, Stream, many, parse, try, (<|>))
import Prelude hiding (and, or)

-- | @(parseFormula s)@ parses a string-like value @s@ as a propositional
-- formula, returning a @'Right' a@ on success, where @a@ is the parsed term.
-- Otherwise, @'Left' e@ is returned, where @e@ is a 'ParseError'.
--
-- __Note:__ Parsing is currently only implemented for the implicational
-- fragment.
--
-- __Conventions__
--
-- * Supported connective symbols:
--
--     * Negation: @"~"@, @"¬"@.
--     * Implication: @"->"@, @"→"@, @"=>"@, @"⇒"@ @"⊃"@.
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
--   quote (a "prime" symbol).
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

formula :: Stream s m Char => ParsecT s u m (Formula String)
formula = padded implication

context :: Stream s m Char => ParsecT s u m (Context String)
context = Set.fromList <$> formula `sepBy` char ','

conclusion :: Stream s m Char => ParsecT s u m (Formula String)
conclusion = turnstile *> formula

sequent :: Stream s m Char => ParsecT s u m (Sequent String)
sequent = (,) <$> context <*> conclusion

implication :: Stream s m Char => ParsecT s u m (Formula String)
implication = foldr1 Imp <$> junction `sepBy1` try rightArrow

-- Conjunctions and disjunctions, read left-to-right
junction :: Stream s m Char => ParsecT s u m (Formula String)
junction = chainl1 negation $ try $ padded $ and <|> or
  where
    and = And <$ wedge
    or = Or <$ vee

negation :: Stream s m Char => ParsecT s u m (Formula String)
negation = try (neg *> (Not <$> term)) <|> term
  where
    term = enclosed formula <|> variable

variable :: Stream s m Char => ParsecT s u m (Formula String)
variable = Var <$> name
  where
    name = (:) <$> nameStart <*> many nameChar
    nameStart = oneOf $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
    nameChar = nameStart <|> oneOf ('\'' : ['0' .. '9'])

-- Ignore space on either side of a parsed string.
padded :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
padded = between spaces spaces

-- Parse a string surrounded by parentheses.
enclosed :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
enclosed = between (char '(') (char ')')

-- Parse an implication arrow.
rightArrow :: Stream s m Char => ParsecT s u m String
rightArrow =
  padded $
    try (string "->")
      <|> string "→"
      <|> try (string "=>")
      <|> string "⇒"
      <|> string "⊃"

-- Parse a conjunction (and) symbol.
wedge :: Stream s m Char => ParsecT s u m String
wedge =
  padded $
    string "&"
      <|> string "∧"
      <|> try (string "/\\")

-- Parse a disjunction (or) symbol.
vee :: Stream s m Char => ParsecT s u m String
vee =
  padded $
    try (string "|" <* notFollowedBy (char '-')) -- needed to handle "|-"
      <|> string "∨"
      <|> try (string "\\/")

neg :: Stream s m Char => ParsecT s u m String
neg = spaces *> (string "~" <|> string "¬")

-- Parse a sequent/judgement turnstile.
turnstile :: Stream s m Char => ParsecT s u m String
turnstile = padded (string "|-" <|> string "⊢")
