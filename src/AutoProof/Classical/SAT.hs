-- |
-- Module      : AutoProof.Classical.SAT
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Satisfiability algorithms.
module AutoProof.Classical.SAT
  ( -- * Truth assignments
    TruthAssignment (evalVar, evalFormula, (|=)),

    -- * Satisfiability algorithms
    naiveSAT,
    naiveSATAssignment,
  )
where

import AutoProof.Classical.SAT.Baseline
  ( naiveSAT,
    naiveSATAssignment,
  )
import AutoProof.Classical.SAT.TruthAssignment
  ( TruthAssignment (evalFormula, evalVar, (|=)),
  )
