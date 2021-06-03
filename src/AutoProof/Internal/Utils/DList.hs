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
-- This is a generalization of the 'ShowS' type.
--
-- See https://wiki.haskell.org/Difference_list.
module AutoProof.Internal.Utils.DList
  ( DList,
    fromList,
    toList,
    toSet,
    cons,
    empty,
    singleton,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set (fromList)

-- | Difference list. See https://wiki.haskell.org/Difference_list.
--
-- Difference lists are concatenated by function composition.
--
-- >>> d1 = fromList [1, 2, 3]
-- >>> d2 = fromList [4, 5, 6]
-- >>> d3 = fromList [7, 8, 9]
-- >>> toList (d1 . d2 . d3)
-- [1,2,3,4,5,6,7,8,9]
type DList a = [a] -> [a]

-- | Convert a list into a difference list.
fromList :: [a] -> DList a
fromList = (++)

-- | Convert a difference list into a list.
toList :: DList a -> [a]
toList = ($ [])

-- | Convert a difference list into a set.
toSet :: Ord a => DList a -> Set a
toSet = Set.fromList . toList

-- | Prepend an element to a difference list.
--
-- >>> toList (cons 0 (fromList [1, 2, 3]))
-- [0,1,2,3]
cons :: a -> DList a -> DList a
cons = (.) . (:)

-- | Empty difference list.
--
-- >>> (toList empty) :: [()]
-- []
empty :: DList a
empty = id

-- | Singleton difference list.
--
-- >>> toList $ (singleton 0) . (singleton 1)
-- [0,1]
singleton :: a -> DList a
singleton = (:)
