-- |
-- Module      : AutoProof.Internal.Utils.MapUtils
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Miscellaneous map-related functions.
module AutoProof.Internal.Utils.MapUtils (toMap) where

import Data.Map (Map)
import qualified Data.Map as Map

-- | Convert a foldable collection of (key, value) pairs into a map.
--
-- >>> set = Set.fromList [("a", 0), ("b", 1)]
-- >>> toMap set
-- fromList [("a",0),("b",1)]
toMap :: (Ord k, Foldable t) => t (k, v) -> Map k v
toMap = foldr (uncurry Map.insert) Map.empty
