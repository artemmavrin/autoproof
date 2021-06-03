-- |
-- Module      : AutoProof.Internal.Classical.SAT.Algorithms.DPLL
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- DPLL satisfiability algorithm.
module AutoProof.Internal.Classical.SAT.Algorithms.DPLL
  ( satDPLL,
    satAssignmentDPLL,
  )
where

import AutoProof.Internal.Classical.CNF (CNF)
import qualified AutoProof.Internal.Classical.CNF as CNF
  ( fromFormula,
    getAnyLiteral,
    pureLiteral,
    substitute,
    unitLiteral,
  )
import AutoProof.Internal.Formula (Formula)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, null)
import Data.Maybe (isJust)
import qualified Data.Set as Set

-- | DPLL satisfiability algorithm.
--
-- ==== __Examples__
--
-- >>> import AutoProof.Internal.Formula
-- >>> satDPLL $ Var "a"
-- True
--
-- >>> satDPLL $ And (Var "a") (Not (Var "a"))
-- False
satDPLL :: Ord a => Formula a -> Bool
satDPLL = isJust . satAssignmentDPLL

-- | DPLL satisfiability algorithm, returning a satisfying truth assihnment, if
-- there is one.
--
-- ==== __Examples__
--
-- >>> a = And (Not (Var "a")) (Or (Var "b") (Var "c")) -- satisfiable
-- >>> Just t = satAssignmentDPLL a
-- >>> t
-- fromList [("a",False),("c",True)]
-- >>> t |= a
-- True
--
-- /Note:/ The variable @"b"@ is missing in the map above, which means that it's
-- implictly assigned the value 'False'.
--
-- >>> satAssignmentDPLL $ And (Var "a") (Not (Var "a")) -- unsatisfiable
-- Nothing
satAssignmentDPLL :: Ord a => Formula a -> Maybe (Map a Bool)
satAssignmentDPLL = satAssignmentDPLLFromCNF . CNF.fromFormula

satAssignmentDPLLFromCNF :: Ord a => CNF a -> Maybe (Map a Bool)
satAssignmentDPLLFromCNF = f Map.empty
  where
    f m cnf
      | Set.null cnf = Just m
      | any Map.null cnf = Nothing
      | otherwise =
        case CNF.unitLiteral cnf of
          Just (x, b) -> f (Map.insert x b m) (CNF.substitute cnf x b)
          Nothing -> case CNF.pureLiteral cnf of
            Just (x, b) -> f (Map.insert x b m) (CNF.substitute cnf x b)
            Nothing -> case CNF.getAnyLiteral cnf of
              Nothing -> Nothing -- should be unreachable
              Just (x, _) ->
                f (Map.insert x True m) (CNF.substitute cnf x True)
                  <|> f (Map.insert x False m) (CNF.substitute cnf x False)
