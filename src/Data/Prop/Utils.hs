{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Data.Prop.Utils
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Miscellaneuous utility functions.
module Data.Prop.Utils where

-- | Class for types that can be "pretty-printed" in a human-readable format.
class PrettyPrintable a where
  pretty :: a -> String

instance PrettyPrintable Char where
  pretty = return

instance PrettyPrintable String where
  pretty = id

instance PrettyPrintable Int where
  pretty = show

instance PrettyPrintable Integer where
  pretty = show
