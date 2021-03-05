{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK hide, prune #-}

-- |
-- Module      : AutoProof.Utils.Parser.Types
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines a lightweight parser type.
module AutoProof.Utils.Parser.Types (Parser (Parser, runParser), parse) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus, (>=>))

-- The parser type and its instances

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ (fmap . fmap) f . p

instance Applicative Parser where
  pure x = Parser $ pure . (,x)

  (Parser p) <*> (Parser p') = Parser $ \s -> do
    (s', f) <- p s
    (s'', x) <- p' s'
    return (s'', f x)

instance Monad Parser where
  (Parser p) >>= f = Parser $ p >=> \(s, x) -> runParser (f x) s

instance Alternative Parser where
  empty = Parser $ const empty

  (<|>) (Parser p) (Parser p') = Parser $ \s -> p s <|> p' s

instance MonadPlus Parser

instance Semigroup a => Semigroup (Parser a) where
  p <> p' = (<>) <$> p <*> p'

instance Monoid a => Monoid (Parser a) where
  mempty = return mempty

-- | Run a parser on a string, discarding any unparsed suffix
parse :: Parser a -> String -> Maybe a
parse (Parser p) = fmap snd . p
