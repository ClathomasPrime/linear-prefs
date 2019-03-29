module Condorcet where

import Data.List
import Control.Monad

import Util
import LinearPref


inInterval :: Ord l => [l] -> [l] -> [l] -> Bool
inInterval x u v =
  all (x `satisfies`) (toOrderedPairs u `intersect` toOrderedPairs v)

condorcetGraphEdges :: Ord l => [(a,[l])] -> [(a, a)]
condorcetGraphEdges prefs = do
  (a,as):rest <- tails prefs
  (b,bs) <- rest
  guard $ 2 == length (filter (\(_,l) -> l `inInterval` as $ bs) prefs)
  return (a,b)
