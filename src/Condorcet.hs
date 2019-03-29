module Condorcet where

import Data.List

import Util
import LinearPref


inInterval :: Ord l => [l] -> [l] -> [l] -> Bool
inInterval x u v =
  all (x `satisfies`) (toOrderedPairs u `intersect` toOrderedPairs v)
