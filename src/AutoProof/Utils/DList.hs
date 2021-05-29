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
module AutoProof.Utils.DList (DList, fromList, toList, cons) where

type DList a = [a] -> [a]

fromList :: [a] -> DList a
fromList = (++)

toList :: DList a -> [a]
toList d = d []

cons :: a -> DList a -> DList a
cons a d = (a :) . d
