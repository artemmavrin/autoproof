-- |
-- Module      : AutoProof.Internal.Classical.CNF
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions and types related to conjunctive normal forms.
module AutoProof.Internal.Classical.CNF
  ( -- * Types
    CNF,
    Clause,
    Literal,

    -- * Conversion functions
    fromFormula,
    toFormula,
    canonicalCNF,

    -- * Operations
    substitute,
    unitLiteral,
    pureLiteral,
    getAnyLiteral,
  )
where

import AutoProof.Internal.Formula
  ( Formula
      ( And,
        Iff,
        Imp,
        Lit,
        Not,
        Or,
        Var
      ),
  )
import qualified AutoProof.Internal.Utils.DList as DList
  ( empty,
    singleton,
    toSet,
  )
import AutoProof.Internal.Utils.MapUtils (toMap)
import Control.Applicative ((<|>))
import qualified Data.List as List (find, lookup)
import Data.Map (Map)
import qualified Data.Map as Map (filterWithKey, lookup, null, toList)
import Data.Set (Set)
import qualified Data.Set as Set (filter, fromList, map, member, toList)

-- | Represents either a propositional variable or its negation, depending on
-- whether the boolean second component is 'True' or 'False', respectively.
type Literal a = (a, Bool)

-- | Represents a disjunction of literals.
type Clause a = Map a Bool

-- | Conjunctive normal form: represents a conjunction of clauses.
type CNF a = Set (Clause a)

-- | Convert a 'Formula' into conjunctive normal form.
--
-- Adapted from Figure 2.6 in
--
-- *  Samuel Mimram (2020)
--    /PROGRAM = PROOF/.
fromFormula :: Ord a => Formula a -> CNF a
fromFormula = Set.map toMap . removeRedundantClauses . convert . pos
  where
    merge a b = concatMap (flip map a . (.)) b

    pos (Lit True) = []
    pos (Lit False) = [DList.empty]
    pos (Var x) = [DList.singleton (x, True)]
    pos (Not a) = neg a
    pos (Imp a b) = merge (neg a) (pos b)
    pos (Or a b) = merge (pos a) (pos b)
    pos (And a b) = pos a ++ pos b
    pos (Iff a b) = pos (And (Imp a b) (Imp b a))

    neg (Lit True) = [DList.empty]
    neg (Lit False) = []
    neg (Var x) = [DList.singleton (x, False)]
    neg (Not a) = pos a
    neg (Imp a b) = pos a ++ neg b
    neg (Or a b) = neg a ++ neg b
    neg (And a b) = merge (neg a) (neg b)
    neg (Iff a b) = neg (And (Imp a b) (Imp b a))

    -- Convert from a list of difference lists of pairs to a set of sets of
    -- pairs.
    convert = Set.fromList . map DList.toSet

    -- Remove redundant clauses from a CNF formula
    removeRedundantClauses = Set.filter notRedundant

    -- Return whether a clause contains a literal and its negation
    notRedundant c = not (any (test c) c)

    -- Test whether a literal and its negation both occur in a clause
    test c (x, b) = (x, not b) `Set.member` c

-- | Convert a conjunctive normal form representation of a formula into a
-- formula.
toFormula :: CNF a -> Formula a
toFormula clauses =
  if any Map.null clauses
    then Lit False
    else case Set.toList clauses of
      [] -> Lit True
      (c : cs) -> foldr (And . clauseToFormula) (clauseToFormula c) cs

clauseToFormula :: Clause a -> Formula a
clauseToFormula literals = case Map.toList literals of
  [] -> Lit False
  (l : ls) -> foldr (Or . literalToFormula) (literalToFormula l) ls

literalToFormula :: Literal a -> Formula a
literalToFormula (x, False) = Not (Var x)
literalToFormula (x, True) = Var x

-- | Convert a formula into a canonical conjunctive normal form.
--
-- ==== __Examples__
--
-- >>> import AutoProof.Internal.Formula
-- >>> canonicalCNF $ Or (Not (Imp (Var "a") (Var "b"))) (Var "c")
-- And (Or (Var "c") (Not (Var "b"))) (Or (Var "c") (Var "a"))
canonicalCNF :: Ord a => Formula a -> Formula a
canonicalCNF = toFormula . fromFormula

-- | Substitute a boolean value for a variable in a CNF formula.
--
-- @('substitute' a x 'True')@ and @('substitute' a x 'False')@ represent the
-- substitutions \(a[\top/x]\) and \(a[\bot/x]\), respectively.
--
-- ==== __Examples__
--
-- >>> a = And (Imp (Var "a") (Not (And (Var "b") (Not (Var "c"))))) (Var "a")
-- >>> cnf = CNF.fromFormula a
-- >>> cnf
-- fromList [fromList [("a",False),("b",False),("c",True)],fromList [("a",True)]]
-- >>> cnf2 = CNF.substitute cnf "a" True
-- >>> cnf2
-- fromList [fromList [("b",False),("c",True)]]
-- >>> a2 = CNF.toFormula cnf2
-- >>> a2
-- Or (Var "c") (Not (Var "b"))
substitute :: Ord a => CNF a -> a -> Bool -> CNF a
substitute a x b =
  Set.map
    (Map.filterWithKey (curry (/= (x, not b))))
    (Set.filter ((/= Just b) . Map.lookup x) a)

-- | Obtain the literal of a unitary clause of a CNF formula, if there is one.
--
-- A /unitary clause/ is one in which exactly one literal occurs.
unitLiteral :: CNF a -> Maybe (Literal a)
unitLiteral = foldr ((<|>) . f) Nothing
  where
    f clause = case Map.toList clause of
      [l] -> Just l
      _ -> Nothing

-- | Obtain a pure literal of a CNF formula, if there is one.
--
-- A /pure literal/ is one which only occurs with the a single polarity in the
-- formula.
pureLiteral :: Ord a => CNF a -> Maybe (Literal a)
pureLiteral a = case List.find isPure variables of
  Just (x, Just b) -> Just (x, b)
  _ -> Nothing
  where
    isPure (_, Just _) = True
    isPure _ = False

    -- Variables together with their polarities
    variables = foldr (findVariables . Map.toList) [] a

    findVariables [] vs = vs
    findVariables ((x, b) : ls) vs = case List.lookup x vs of
      -- We previously encountered x with just the polarity b
      Just (Just b')
        -- The current polarity agrees with the previous polarity, so we keep
        -- looking
        | b' == b -> findVariables ls vs
        -- We now saw both polarities
        | otherwise -> findVariables ls ((x, Nothing) : vs')
        where
          vs' = filter (\(y, _) -> y /= x) vs
      -- We previously encountered both polarities of x
      Just Nothing -> findVariables ls vs
      -- This is the first time we see x
      Nothing -> findVariables ls ((x, Just b) : vs)


-- | Obtain a literal from a CNF formula, if there is one.
getAnyLiteral :: CNF a -> Maybe (Literal a)
getAnyLiteral = f . Set.toList
  where
    f [] = Nothing
    f (c : cs) = case Map.toList c of
      [] -> f cs
      l : _ -> Just l
