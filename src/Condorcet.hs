module Condorcet where

import Data.List
import Data.Maybe
import Control.Monad

import Util
import LinearPref
import Voting


-- NOTE: I keep running this on *non-closed* condorcet domains.
-- This might mean things get screwed up, e.g. not median graphs,
-- and may be screwing up my intuition.
--
-- How I would like to go forward:
--   - look at as many of the meaningful separations of pref classes as possible
--   - and at the condorcet graphs of all the resulting examples.

inInterval :: Ord l => [l] -> [l] -> [l] -> Bool
inInterval u v x =
  all (x `satisfies`) (toOrderedPairs u `intersect` toOrderedPairs v)

interval :: Ord l => [l] -> [l] -> [[l]]
interval u v = filter (inInterval u v) (permutations $ sort u)

condorcetGraphEdges' :: Ord l => [[l]] -> [([l], [l])]
condorcetGraphEdges' prefs = condorcetGraphEdges $ fmap (\x -> (x,x)) prefs

condorcetGraphEdges :: Ord l => [(a,[l])] -> [(a, a)]
condorcetGraphEdges prefs = do
  (a,as):rest <- tails prefs
  (b,bs) <- rest
  guard $ 2 == length (filter (inInterval as bs . snd) prefs)
  return (a,b)

isoGraph :: Ord a => [a] -> [(a,a)] -> [(a,a)] -> Bool
isoGraph labels gr1 gr2 =
  length gr1 == length gr2 && any works permFuncs
  where works perm = gr1 `eqAsEdgeSet` fmap (both perm) gr2
        permFuncs = fmap funcOf (permutations labels)
        funcOf perm i = perm !! fromJust (i `elemIndex` labels)

eqAsEdgeSet :: Ord a => [(a,a)] -> [(a,a)] -> Bool
eqAsEdgeSet gr1 gr2 = eqAsSet (s gr1) (s gr2)
  where s = fmap sortPair
        sortPair (a, b) = (max a b, min a b)

-- this suffices because math
isCondorcet :: Ord l => [[l]] -> Bool
isCondorcet = isJust . condorcetExpand

condorcetExpand :: Ord l => [[l]] -> Maybe [[l]]
condorcetExpand prefs = fmap (nubSort . (prefs++)) . sequence $ do
  x:ys <- tails prefs
  y:zs <- tails ys
  z <- zs
  return $ majorityRuleSafe [x,y,z]

condorcetClosure :: Ord l => [[l]] -> Maybe [[l]]
condorcetClosure ls =
  case condorcetExpand ls of
    Nothing -> Nothing
    Just ls' -> if ls == ls' then Just ls' else condorcetClosure ls'

-- hasse coverers in the collection of closed condorcets (ordered by inclusion)
condorcetSuccessors :: Ord l => [[l]] -> [ [[l]] ]
condorcetSuccessors prefs =
  let outcomes = if null prefs
        then error "empty pref cond succ"
        else sort . head $ prefs
      allPrefs = permutations outcomes
      accum (succesors, []) = (succesors, [])
      accum (succesors, pr:prs) =
        case condorcetClosure (pr:prefs) of
          Nothing -> accum (succesors, prs)
          Just biggerDomain -> (biggerDomain:succesors, prs \\ biggerDomain)
   in drop 1 . fst $ until (null . snd) accum ([], allPrefs \\ prefs)

isMaximal :: Ord l => [[l]] -> Bool
isMaximal = null . condorcetSuccessors


{-
tripin :: Ord l => [[l]] -> [(l,l,l)]
tripin prefs = nub $ do
  x:ys <- tails . sort . head $ prefs
  y:zs <- tails ys
  z <- zs
  return (x,y,z)
  -}

gr2 = [('a','b'),('a','c'),('a','h'),('b','d'),('b','f'),('c','d'),('c','g'),('d','e'),('e','f'),('e','g'),('f','h'),('g','h')]
gr1 = [('a','e'),('a','f'),('a','h'),('b','c'),('b','d'),('b','g'),('c','e'),('c','h'),('d','e'),('d','f'),('f','g'),('g','h')]
iso i = case i of
  'a' -> 'f'
  'b' -> 'd'
  'c' -> 'a'
  'd' -> 'e'
  'e' -> 'c'
  'f' -> 'b'
  'g' -> 'h'
  'h' -> 'g'

