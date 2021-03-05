-- |
-- Module      : AutoProof.Utils.Parser
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Provides a lightweight, general-purpose parser type and some related
-- combinators.
module AutoProof.Utils.Parser
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

import AutoProof.Utils.Parser.Char
  ( anyChar,
    char,
    charIf,
    oneOf,
    spaces,
    string,
  )
import AutoProof.Utils.Parser.Combinator
  ( between,
    chainl1,
    chainr1,
    eof,
    notFollowedBy,
    sepBy,
    sepBy1,
  )
import AutoProof.Utils.Parser.Types (Parser, parse)
