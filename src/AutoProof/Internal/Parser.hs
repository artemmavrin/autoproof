{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : AutoProof.Internal.Parser
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Parser for propositional logic formulas
module AutoProof.Internal.Parser
  ( -- * Safe parsing
    parseFormula,
    parseJudgement,

    -- * Unsafe parsing
    unsafeParseFormula,
    unsafeParseJudgement,
  )
where

import AutoProof.Internal.Formula (Formula (And, Iff, Imp, Lit, Not, Or, Var))
import AutoProof.Internal.Judgement
  ( Judgement,
    (|-),
  )
import AutoProof.Internal.Utils.Parser
  ( Parser,
    between,
    chainl1,
    chainr1,
    char,
    eof,
    notFollowedBy,
    oneOf,
    parse,
    sepBy,
    spaces,
    string,
  )
import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Maybe (fromMaybe)

-- | @('parseFormula' s)@ parses a string @s@ as a propositional formula,
-- returning a @'Just' a@ on success, where @a@ is the parsed formula.
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
--     * Disjunction: @"|"@, @"\\/"@, @"∨"@, @"+"@
--     * Conjunction: @"&"@, @"/\\"@, @"∧"@, @"^"@, @"*"@
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
-- Just (Imp (Var "a") (Imp (Var "b") (Var "c")))
--
-- >>> parseFormula "~a | b -> c"
-- Just (Imp (Or (Not (Var "a")) (Var "b")) (Var "c"))
--
-- >>> parseFormula "(a -> b) & ~c"
-- Just (And (Imp (Var "a") (Var "b")) (Not (Var "c")))
parseFormula :: String -> Maybe (Formula String)
parseFormula = parse (formula <* eof)

-- | @(parseJudgement s)@ parses a string @s@ as a propositional judgement.
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
-- Just ([Var "a",Imp (Var "a") (Var "b")] |- Var "b")
--
-- >>> parseJudgement "a |- a | b"
-- Just ([Var "a"] |- Or (Var "a") (Var "b"))
parseJudgement :: String -> Maybe (Judgement String)
parseJudgement = parse (judgement <* eof)

-- | @(unsafeParseFormula s)@ parses a string @s@ as a propositional formula,
-- returning the parsed formula on success, and throwing an error on failure.
--
-- See 'parseFormula' for grammar spcecifications.
--
-- ==== __Examples__
--
-- >>> unsafeParseFormula "(a => b) => c"
-- Imp (Imp (Var "a") (Var "b")) (Var "c")
unsafeParseFormula :: String -> Formula String
unsafeParseFormula = fromMaybe (error "Parse error") . parseFormula

-- | @(unsafeParseJudgement s)@ parses a string @s@ as a propositional
-- judgement, returning the parsed judgement on success, and throwing an error
-- on failure.
--
-- See 'parseJudgement' for the specification of valid judgements.
--
-- ==== __Examples__
--
-- >>> unsafeParseJudgement "a, b |- a -> b"
-- [Var "a",Var "b"] |- Imp (Var "a") (Var "b")
--
-- >>> unsafeParseJudgement "a & b |- a"
-- [And (Var "a") (Var "b")] |- Var "a"
unsafeParseJudgement :: String -> Judgement String
unsafeParseJudgement = fromMaybe (error "Parse error") . parseJudgement

-- Top-level parsers

formula :: Parser (Formula String)
formula = padded implication

context :: Parser [Formula String]
context = formula `sepBy` char ','

judgement :: Parser (Judgement String)
judgement = (|-) <$> context <*> (spaces *> turnstileS *> formula)

-- Special symbols

-- Generate a parser out of a collection of strings. The parser will parse any
-- one of the given strings.
symbol :: [String] -> Parser String
symbol [] = empty
symbol (c : cs)
  -- Special case needed to handle the turnstile symbol "|-" separately from the
  -- disjunction symbol "|".
  | c == "|" = (string "|" <* notFollowedBy (char '-')) <|> symbol cs
  | otherwise = string c <|> symbol cs

impS :: Parser String
impS = symbol ["->", "=>", "→", "⇾", "⇒", "⊃"]

iffS :: Parser String
iffS = symbol ["<->", "↔", "⇿", "⇔"]

andS :: Parser String
andS = symbol ["&", "^", "∧", "/\\", "*"]

orS :: Parser String
orS = symbol ["|", "∨", "\\/", "+"]

notS :: Parser String
notS = symbol ["~", "¬"]

falseS :: Parser String
falseS = symbol ["⊥", "false"]

trueS :: Parser String
trueS = symbol ["⊤", "true"]

turnstileS :: Parser String
turnstileS = symbol ["|-", "⊢"]

-- Propositional formula components

-- Right-associative nested implications
implication :: Parser (Formula String)
implication = chainr1 equivalence (padded (Imp <$ impS))

-- Left-associative nested equivalences
equivalence :: Parser (Formula String)
equivalence = chainl1 disjunction (padded (Iff <$ iffS))

-- Left-associative nested disjunctions
disjunction :: Parser (Formula String)
disjunction = chainl1 conjunction (padded (Or <$ orS))

-- Left-associative nested conjunctions
conjunction :: Parser (Formula String)
conjunction = chainl1 negation (padded (And <$ andS))

-- Zero or more consecutive negations
negation :: Parser (Formula String)
negation = spaces *> (foldl (.) id <$> many (Not <$ notS) <*> atom)

atom :: Parser (Formula String)
atom = enclosed formula <|> false <|> true <|> variable

false :: Parser (Formula String)
false = Lit False <$ falseS

true :: Parser (Formula String)
true = Lit True <$ trueS

variable :: Parser (Formula String)
variable = Var <$> name
  where
    name = (:) <$> nameStart <*> many nameChar
    nameStart = oneOf $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
    nameChar = nameStart <|> oneOf ('\'' : ['0' .. '9'])

-- Helper functions

padded :: Parser a -> Parser a
padded = between spaces spaces

enclosed :: Parser a -> Parser a
enclosed = between (char '(') (char ')')
