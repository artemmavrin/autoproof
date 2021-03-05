{-# OPTIONS_HADDOCK hide, prune #-}

-- |
-- Module      : AutoProof.Utils.Parser.Combinator
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines parser combinators.
module AutoProof.Utils.Parser.Combinator
  ( between,
    sepBy,
    sepBy1,
    chainl1,
    chainr1,
    notFollowedBy,
    eof,
  )
where

import AutoProof.Utils.Parser.Char (anyChar)
import AutoProof.Utils.Parser.Types (Parser (Parser))
import Control.Applicative (Alternative (many, (<|>)))

between :: Parser b -> Parser c -> Parser a -> Parser a
between l r p = l *> p <* r

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = p `sepBy1` sep <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- chainr1 p op; rest (f x y)) <|> return x

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser q
  where
    q s = case p s of
      Nothing -> Just (s, ())
      Just _ -> Nothing

eof :: Parser ()
eof = notFollowedBy anyChar
