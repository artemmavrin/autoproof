-- |
-- Module      : AutoProof.Internal.Classical.SAT.Algorithms
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Satisfiability algorithms.
module AutoProof.Internal.Classical.SAT.Algorithms
  ( -- * Simple baseline algorithm
    simpleSAT,
    simpleSATAssignment,
  )
where

import AutoProof.Internal.Classical.SAT.Algorithms.Simple
  ( simpleSAT,
    simpleSATAssignment,
  )
