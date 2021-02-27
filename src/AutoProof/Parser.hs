{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : AutoProof.Parser
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Parser for propositional logic formulas
module AutoProof.Parser
  ( -- * Safe parsing
    parseFormula,
    parseJudgement,

    -- * Unsafe parsing
    unsafeParseFormula,
    unsafeParseJudgement,
  )
where

import AutoProof.Formula
  ( Formula,
    and,
    iff,
    imp,
    lit,
    not,
    or,
    var,
  )
import AutoProof.Judgement
  ( Judgement,
    (|-),
  )
import Data.Functor.Identity (Identity)
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
import Prelude hiding (and, not, or)

-- | @('parseFormula' s)@ parses a string-like value @s@ as a propositional
-- formula, returning a @'Right' a@ on success, where @a@ is the parsed formula.
-- Otherwise, @'Left' e@ is returned, where @e@ is a @'ParseError'@.
--
-- For a version of 'parseFormula' that does not return a wrapped formula but
-- also throws an error when it cannot parse, see 'unsafeParseFormula'.
--
-- ==== __Conventions__
--
-- * Supported connective symbols:
--
--     * Negation: @"~"@, @"¬"@.
--     * Implication: @"->"@, @"=>"@, @"→"@, @"⇾"@, @"⇒"@ @"⊃"@.
--     * Disjunction: @"|"@, @"\\/"@, @"∨"@
--     * Conjunction: @"&"@, @"/\\"@, @"∧"@, @"^"@
--     * Equivalence: @"\<-\>"@, @"↔"@, @"⇿"@, @"⇔"@
--
-- * Implication is right-associative and has lower precedence than the other
--   connectives.
-- * Equivalence is left-associative and has higher precedence than implication
--   but lower precedence than conjunction and disjunction.
-- * Disjunction is left-associative and has higher precedence than equivalence
--   but lower precedence than conjunction.
-- * Conjunction is left-associative and has highest precedence out of the
--   binary connectives.
-- * Negation binds most tightly, and must immediately precede its argument
--   (i.e., there should not be a space between a negation symbol and the
--   proposition that follows).
-- * Valid variable names begin with a letter (uppercase or lowercase) or an
--   underscore, and may be followed by a letter, underscore, digit, or single
--   quote (a "prime" symbol). The exceptions are the strings @"false"@ and
--   @"true"@, which are parsed as the propositional literals \(\bot\) and
--   \(\top\), respectively.
--
-- ==== __Examples__
--
-- >>> parseFormula "a -> b -> c"
-- Right (imp (var "a") (imp (var "b") (var "c")))
--
-- >>> parseFormula "~a | b -> c"
-- Right (imp (or (not (var "a")) (var "b")) (var "c"))
--
-- >>> parseFormula "(a -> b) & ~c"
-- Right (and (imp (var "a") (var "b")) (not (var "c")))
parseFormula :: Stream s Identity Char => s -> Either ParseError (Formula String)
parseFormula = parse (formula <* eof) ""

-- | @(parseJudgement s)@ parses a string-like value @s@ as a propositional
-- judgement.
--
-- A valid judgement is made up of an antecedent, a turnstile symbol, and a
-- consequent (in that order). The antecedents are a (potentially empty)
-- comma-separated list of formulas, the turnstile symbol is either @"|-"@ or
-- @"⊢"@, and the consequent is another formula.
--
-- See 'parseFormula' for the specification of valid formulas.
--
-- ==== __Examples__
--
-- >>> parseJudgement "a, a -> b |- b"
-- Right ([var "a",imp (var "a") (var "b")] |- var "b")
--
-- >>> parseJudgement "a |- a | b"
-- Right ([var "a"] |- or (var "a") (var "b"))
parseJudgement :: Stream s Identity Char => s -> Either ParseError (Judgement String)
parseJudgement = parse (judgement <* eof) ""

-- | @(unsafeParseFormula s)@ parses a string-like value @s@ as a propositional
-- formula, returning the parsed formula on success, and throwing an error on
-- failure.
--
-- See 'parseFormula' for grammar spcecifications.
--
-- ==== __Examples__
--
-- >>> unsafeParseFormula "(a => b) => c"
-- imp (imp (var "a") (var "b")) (var "c")
unsafeParseFormula :: Stream s Identity Char => s -> Formula String
unsafeParseFormula = either (error . show) id . parseFormula

-- | @(unsafeParseJudgement s)@ parses a string-like value @s@ as a
-- propositional judgement, returning the parsed judgement on success, and
-- throwing an error on failure.
--
-- See 'parseJudgement' for the specification of valid judgements.
--
-- ==== __Examples__
--
-- >>> unsafeParseJudgement "a, b |- a -> b"
-- [var "a",var "b"] |- imp (var "a") (var "b")
--
-- >>> unsafeParseJudgement "a & b |- a"
-- [and (var "a") (var "b")] |- var "a"
unsafeParseJudgement :: Stream s Identity Char => s -> Judgement String
unsafeParseJudgement = either (error . show) id . parseJudgement

-- Top-level parsers

formula :: Stream s m Char => ParsecT s u m (Formula String)
formula = padded implication

context :: Stream s m Char => ParsecT s u m [Formula String]
context = formula `sepBy` char ','

judgement :: Stream s m Char => ParsecT s u m (Judgement String)
judgement = (|-) <$> context <*> (spaces *> turnstileS *> formula)

-- Special symbols

-- Generate a parser out of a collection of strings. The parser will parse any
-- one of the given strings.
symbol :: Stream s m Char => [String] -> ParsecT s u m String
symbol [] = parserZero
symbol (c : cs)
  -- Special case needed to handle the turnstile symbol "|-" separately from the
  -- disjunction symbol "|".
  | c == "|" = try (string "|" <* notFollowedBy (char '-')) <|> symbol cs
  | otherwise = try (string c) <|> symbol cs

impS :: Stream s m Char => ParsecT s u m String
impS = symbol ["->", "=>", "→", "⇾", "⇒", "⊃"]

iffS :: Stream s m Char => ParsecT s u m String
iffS = symbol ["<->", "↔", "⇿", "⇔"]

andS :: Stream s m Char => ParsecT s u m String
andS = symbol ["&", "^", "∧", "/\\"]

orS :: Stream s m Char => ParsecT s u m String
orS = symbol ["|", "∨", "\\/"]

notS :: Stream s m Char => ParsecT s u m String
notS = symbol ["~", "¬"]

falseS :: Stream s m Char => ParsecT s u m String
falseS = symbol ["⊥", "false"]

trueS :: Stream s m Char => ParsecT s u m String
trueS = symbol ["⊤", "true"]

turnstileS :: Stream s m Char => ParsecT s u m String
turnstileS = symbol ["|-", "⊢"]

-- Propositional formula components

-- Right-associative nested implications
implication :: Stream s m Char => ParsecT s u m (Formula String)
implication = chainr1 equivalence (try $ padded (imp <$ impS))

-- Left-associative nested equivalences
equivalence :: Stream s m Char => ParsecT s u m (Formula String)
equivalence = chainl1 disjunction (try $ padded (iff <$ iffS))

-- Left-associative nested disjunctions
disjunction :: Stream s m Char => ParsecT s u m (Formula String)
disjunction = chainl1 conjunction (try $ padded (or <$ orS))

-- Left-associative nested conjunctions
conjunction :: Stream s m Char => ParsecT s u m (Formula String)
conjunction = chainl1 negation (try $ padded (and <$ andS))

-- Zero or more consecutive negations
negation :: Stream s m Char => ParsecT s u m (Formula String)
negation = spaces *> (foldl (.) id <$> try (many (not <$ notS)) <*> atom)

atom :: Stream s m Char => ParsecT s u m (Formula String)
atom = enclosed formula <|> try false <|> try true <|> variable

false :: Stream s m Char => ParsecT s u m (Formula String)
false = lit False <$ falseS

true :: Stream s m Char => ParsecT s u m (Formula String)
true = lit True <$ trueS

variable :: Stream s m Char => ParsecT s u m (Formula String)
variable = var <$> name
  where
    name = (:) <$> nameStart <*> many nameChar
    nameStart = oneOf $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
    nameChar = nameStart <|> oneOf ('\'' : ['0' .. '9'])

-- Helper functions

padded :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
padded = between spaces spaces

enclosed :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
enclosed = between (char '(') (char ')')
