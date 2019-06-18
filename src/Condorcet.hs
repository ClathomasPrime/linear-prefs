module Condorcet where

import Data.List
import Data.Maybe
import Control.Monad

import Util
import LinearPref
import Voting

import Debug.Trace


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

showGr :: [([Int], [Int])] -> [(String, String)]
showGr = fmap (both $ concat . fmap show)

sameDegreeMultiset :: Ord a => [a] -> [(a,a)] -> [(a,a)] -> Bool
sameDegreeMultiset labels gr1 gr2 =
  length gr1 == length gr2 &&
    degrees gr1 `eqUpToOrder` degrees gr2
  where degrees g = fmap (\i -> length . filter (has i) $ g) labels
        has i (u, v) = i == u || i == v

isoGraph :: Ord a => [a] -> [(a,a)] -> [(a,a)] -> Bool
isoGraph labels gr1 gr2 =
  length gr1 == length gr2
    && degrees gr1 `eqUpToOrder` degrees gr2
    && any works (traceShow (length permFuncs) permFuncs)
  where degrees g = fmap (\i -> length . filter (has i) $ g) labels
        has i (u, v) = i == u || i == v
        works perm = gr1 `eqAsEdgeSet` fmap (both perm) gr2
        permFuncs = fmap funcOf (permutations labels)
        funcOf perm i = perm !! fromJust (i `elemIndex` labels)

eqAsEdgeSet :: Ord a => [(a,a)] -> [(a,a)] -> Bool
eqAsEdgeSet gr1 gr2 = eqUpToOrder (s gr1) (s gr2)
  where s = fmap sortPair
        sortPair (a, b) = (max a b, min a b)

neighbors :: Eq a => a -> [(a,a)] -> [a]
neighbors a graph = nub . concat . fmap phi $ graph
  where phi (u,v)
          | a == u = [v]
          | a == v = [u]
          | otherwise = []

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
          Nothing -> (succesors, prs)
          Just biggerDomain -> (biggerDomain:succesors, prs ) -- \\ biggerDomain)
   in fst $ until (null . snd) accum ([], allPrefs \\ prefs)

maximalExtensions :: (Show l, Ord l) => [[l]] -> [ [[l]] ]
maximalExtensions seedPrefs = fst $ until (null . snd) looper ([], [sort seedPrefs])
  where looper (maxExts, frontier)
          = -- trace (show $ length frontier) $
            both nubSort $ foldl accum (maxExts, []) frontier
        accum (maxExts, newFrontier) prefs
          = case condorcetSuccessors prefs of
              [] -> (prefs : maxExts, newFrontier)
              successors -> (maxExts, fmap sort successors ++ newFrontier)


isClosed :: Ord l => [[l]] -> Bool
isClosed prefs = not $ any (`notElem` prefs) majorities
  where majorities = fmap (\(x,y,z) -> majorityRule [x,y,z]) (allTriples prefs)

isMaximal :: Ord l => [[l]] -> Bool
isMaximal prefs = isClosed prefs && not (any isCondorcet addOnes)
  where addOnes = fmap (:prefs) (permutations (head prefs) \\ prefs)

isConnected :: Ord l => [[l]] -> Bool
isConnected prefs = all (uncurry adj) (condorcetGraphEdges' prefs)
  where adj p1 p2 = 2 == length (interval p1 p2)

isNormal :: Eq l => [[l]] -> Bool
isNormal prefs = any (uncurry reversed) pairs
  where reversed u v = u == reverse v
        pairs = [(p,q) | p:ps <- tails prefs, q <- ps]

isSymmetric :: Eq l => [[l]] -> Bool
isSymmetric prefs = all (\p -> any (reversed p) prefs) prefs
  where reversed u v = u == reverse v

edgeQuotientTest :: Ord l => [l] -> [l] -> l -> Bool
edgeQuotientTest p q a =
  nub (fmap (filter (/= a)) $ interval p q)
  `eqAsSet` interval (filter (/= a) p) (filter (/= a) q)

{-
tripin :: Ord l => [[l]] -> [(l,l,l)]
tripin prefs = nub $ do
  x:ys <- tails . sort . head $ prefs
  y:zs <- tails ys
  z <- zs
  return (x,y,z)
  -}

-- gr2 = [('a','b'),('a','c'),('a','h'),('b','d'),('b','f'),('c','d'),('c','g'),('d','e'),('e','f'),('e','g'),('f','h'),('g','h')]
-- gr1 = [('a','e'),('a','f'),('a','h'),('b','c'),('b','d'),('b','g'),('c','e'),('c','h'),('d','e'),('d','f'),('f','g'),('g','h')]
-- iso i = case i of
--   'a' -> 'f'
--   'b' -> 'd'
--   'c' -> 'a'
--   'd' -> 'e'
--   'e' -> 'c'
--   'f' -> 'b'
--   'g' -> 'h'
--   'h' -> 'g'

