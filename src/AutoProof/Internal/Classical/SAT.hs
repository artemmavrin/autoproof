-- |
-- Module      : AutoProof.Internal.Classical.SAT
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Satisfiability algorithms.
module AutoProof.Internal.Classical.SAT
  ( -- * Truth assignments
    TruthAssignment (evalVar, evalFormula),
    (|=),

    -- * Satisfiability algorithms
    simpleSAT,
    simpleSATAssignment,
  )
where

import AutoProof.Internal.Classical.SAT.Algorithms
  ( simpleSAT,
    simpleSATAssignment,
  )
import AutoProof.Internal.Classical.SAT.TruthAssignment
  ( TruthAssignment (evalFormula, evalVar),
    (|=),
  )
