-- |
-- Module      : AutoProof.Proof.Provability
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Check provability of general propositional formulas
module AutoProof.Proof.Provability (toImp, isTautology) where

import AutoProof.Formula
  ( Formula (And, Iff, Imp, Lit, Not, Or, Var),
    false,
    imp,
    subformulas,
    true,
    var,
  )
import AutoProof.Judgement (Judgement (Judgement))
import AutoProof.Proof.Implication (proveImp)
import AutoProof.Utils.Set (toSet)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Prelude hiding (and, not, or)

-- | Convert a propositional formula into an implicational judgement which is
-- provable if and only if the original formula is an intuitionistic tautology.
--
-- This construction is due to
--
-- *  Richard Statman (1979)
--    "Intuitionistic propositional logic is polynomial-space complete."
--    Theoretical Computer Science, Volume 9, Issue 1, pp. 67–72.
--    <https://doi.org/10.1016/0304-3975(79)90006-9 DOI>.
toImp :: Ord a => Formula a -> Judgement (Either (Formula a) a)
toImp a = Judgement (f Set.empty a) (new a)
  where
    -- TODO: explain what's going on here
    new = var . Left

    old = var . Right

    subs = subformulas a

    common b = toSet [imp (new false) (new b), imp (new b) (new true)]

    unique = toSet . unique'

    unique' (Lit True) = [new true]
    unique' (Lit False) = []
    unique' r@(Var x) =
      [ imp (old x) (new r),
        imp (new r) (old x)
      ]
    unique' r@(Not _ _ p) =
      [ imp (new r) (imp (new p) (new false)),
        imp (imp (new p) (new false)) (new r)
      ]
    unique' r@(Imp _ _ p q) =
      [ imp (new r) (imp (new p) (new q)),
        imp (imp (new p) (new q)) (new r)
      ]
    unique' r@(Or _ _ p q) =
      let u0 =
            [ imp (new p) (new r),
              imp (new q) (new r)
            ]
          orE t = imp (new r) (imp (imp (new p) (new t)) (imp (imp (new q) (new t)) (new t)))
       in foldr ((:) . orE) u0 subs
    unique' r@(And _ _ p q) =
      [ imp (new p) (imp (new q) (new r)),
        imp (new r) (new p),
        imp (new r) (new q)
      ]
    unique' r@(Iff _ _ p q) =
      [ imp (new r) (imp (new p) (new q)),
        imp (new r) (imp (new q) (new p)),
        imp (imp (new p) (new q)) (imp (imp (new q) (new p)) (new r))
      ]

    f c b = g (Set.union c (common b)) b

    g c r@(Lit _) = Set.union c (unique r)
    g c r@(Var _) = Set.union c (unique r)
    g c r@(Not _ _ p) = f (Set.union c (unique r)) p
    g c r@(Imp _ _ p q) = f (f (Set.union c (unique r)) p) q
    g c r@(Or _ _ p q) = f (f (Set.union c (unique r)) p) q
    g c r@(And _ _ p q) = f (f (Set.union c (unique r)) p) q
    g c r@(Iff _ _ p q) = f (f (Set.union c (unique r)) p) q

-- | Determine whether a formula is an intuitionistic tautology.
--
-- The algorithm is due to
--
-- *  Richard Statman (1979)
--    "Intuitionistic propositional logic is polynomial-space complete."
--    Theoretical Computer Science, Volume 9, Issue 1, pp. 67–72.
--    <https://doi.org/10.1016/0304-3975(79)90006-9 DOI>.
--
-- >>> isTautology $ imp (and (var 'a') (var 'b')) (var 'a')
-- True
--
-- >>> isTautology $ or (var 'a') (not (var 'a'))
-- False
--
-- >>> isTautology $ not (not (or (var 'a') (not (var 'a'))))
-- True
isTautology :: Ord a => Formula a -> Bool
isTautology = isJust . proveImp . toImp
