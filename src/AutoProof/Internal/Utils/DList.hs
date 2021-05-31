{-# OPTIONS_HADDOCK hide, prune #-}

-- |
-- Module      : AutoProof.Internal.Utils.DList
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Simple difference list implementation. Difference lists are an abstraction
-- that enables constant-time concatenation of lists using function composition.
module AutoProof.Internal.Utils.DList
  ( DList,
    fromList,
    toList,
    toSet,
    cons,
    empty,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set (fromList)

type DList a = [a] -> [a]

fromList :: [a] -> DList a
fromList = (++)

toList :: DList a -> [a]
toList = ($ [])

toSet :: Ord a => DList a -> Set a
toSet = Set.fromList . toList

cons :: a -> DList a -> DList a
cons = (.) . (:)

empty :: DList a
empty = id
