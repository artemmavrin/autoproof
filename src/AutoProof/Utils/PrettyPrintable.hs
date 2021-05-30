{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : AutoProof.Utils.PrettyPrintable
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines the 'PrettyPrintable' class.
module AutoProof.Utils.PrettyPrintable
  ( PrettyPrintable (pretty, prettys),
    prettySeq,
    prettysSeq,
  )
where

import qualified AutoProof.Utils.DList as DList (fromList, toList)
import Data.Foldable (toList)

-- | Class for types that can be "pretty-printed" in a human-readable format.
class PrettyPrintable a where
  {-# MINIMAL pretty | prettys #-}

  -- | Pretty-print a value.
  pretty :: a -> String
  pretty = DList.toList . prettys

  -- | Difference-list representation of a pretty-printed value.
  prettys :: a -> ShowS
  prettys = DList.fromList . pretty

instance PrettyPrintable Char where
  prettys = showChar

instance PrettyPrintable String where
  prettys = showString

instance PrettyPrintable Int where
  prettys = shows

instance PrettyPrintable Integer where
  prettys = shows

-- | Difference-list representation of a pretty-printed collection of
-- pretty-printable values.
prettysSeq :: (Foldable t, PrettyPrintable a) => t a -> ShowS
prettysSeq c s = case toList c of
  [] -> s
  (p : ps) -> prettys p (f ps)
  where
    f [] = s
    f (q : qs) = ',' : ' ' : prettys q (f qs)

-- | Pretty-print a collection of pretty-printable values.
prettySeq :: (Foldable t, PrettyPrintable a) => t a -> String
prettySeq c = prettysSeq c ""
