{-# OPTIONS_HADDOCK hide, prune #-}

-- |
-- Module      : AutoProof.Utils.Set
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Miscellaneous set-related functions.
module AutoProof.Utils.Set (toSet) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Convert a foldable collection into a set.
toSet :: (Ord a, Foldable t) => t a -> Set a
toSet = foldr Set.insert Set.empty
