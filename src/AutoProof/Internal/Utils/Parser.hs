{-# OPTIONS_HADDOCK hide, prune #-}

-- |
-- Module      : AutoProof.Internal.Utils.Parser
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Provides a lightweight, general-purpose parser type and some related
-- combinators.
--
-- The API is deliberately nearly a subset of the @parsec@ API. One big
-- difference is the behavior of the 'Control.Applicative.Alternative' instace;
-- our instance always backtracks, so there is no need for a @try@ function.
module AutoProof.Internal.Utils.Parser
  ( -- * The Parser type
    Parser,

    -- * Running a parser
    parse,

    -- * Parser constructors
    charIf,
    char,
    anyChar,
    oneOf,
    string,
    spaces,

    -- * Parser combinators
    between,
    sepBy,
    sepBy1,
    chainl1,
    chainr1,
    notFollowedBy,
    eof,
  )
where

import AutoProof.Internal.Utils.Parser.Char
  ( anyChar,
    char,
    charIf,
    oneOf,
    spaces,
    string,
  )
import AutoProof.Internal.Utils.Parser.Combinator
  ( between,
    chainl1,
    chainr1,
    eof,
    notFollowedBy,
    sepBy,
    sepBy1,
  )
import AutoProof.Internal.Utils.Parser.Types (Parser, parse)
