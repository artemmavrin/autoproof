{-# OPTIONS_HADDOCK hide, prune #-}

-- |
-- Module      : AutoProof.Utils.DList
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Simple difference list implementation. Difference lists are an abstraction
-- that enables constant-time concatenation of lists using function composition.
module AutoProof.Utils.DList (DList, toDList, fromDList) where

type DList a = [a] -> [a]

toDList :: [a] -> DList a
toDList = (++)

fromDList :: DList a -> [a]
fromDList d = d []
