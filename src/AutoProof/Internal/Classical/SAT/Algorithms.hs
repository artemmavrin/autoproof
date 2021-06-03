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
    satSimple,
    satAssignmentSimple,

    -- * DPLL
    satDPLL,
    satAssignmentDPLL,
  )
where

import AutoProof.Internal.Classical.SAT.Algorithms.DPLL
  ( satAssignmentDPLL,
    satDPLL,
  )
import AutoProof.Internal.Classical.SAT.Algorithms.Simple
  ( satAssignmentSimple,
    satSimple,
  )
