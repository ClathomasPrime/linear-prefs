{-# LANGUAGE TupleSections #-}
module ConnectedGraph where

-- import Util
-- import GeneralD
import LinearPref
import Rules
-- import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Tuple
import Data.Maybe

import Debug.Trace

type Graph x = Map x [x]

pairImplicationGraph :: Ord l => [l] -> [[l]] -> Graph (l,l)
pairImplicationGraph outcomes prefs =
  let (_, freePairs) = separateDominatedPairs outcomes prefs
      conclusionsFrom p = intersect (conclusionsThatHold [p] prefs) freePairs \\ [p]
   in Map.fromList [ (p, conclusionsFrom p) | p <- freePairs ]

separateDominatedPairs :: Ord l => [l] -> [[l]] -> ([(l,l)], [(l,l)])
separateDominatedPairs outcomes prefs
  = (filter fixed pairs, filter free pairs)
  where fixed (a,b) = all (\pref -> greaterByPref pref a b) prefs
        pairs = [(l,l') | l <- outcomes, l' <- outcomes, l /= l']
        free p = not (fixed p) && not (fixed $ swap p)

componentOf :: Ord a => a -> Graph a -> Graph a
componentOf start graph = subgraph reachable graph
  where reachable k = k `elem` expand [] [start]
        expand accum [] = accum
        expand accum (v:vs)
          = expand (v:accum) ((neighbors v graph ++ vs) \\ accum)

subgraph :: Ord a => (a -> Bool) -> Graph a -> Graph a
subgraph pred graph =
  fmap (filter pred) . Map.filterWithKey (\k _ -> pred k) $ graph

neighbors :: Ord a => a -> Graph a -> [a]
neighbors a graph = union succs preds
  where Just succs = Map.lookup a graph
        preds = Map.keys . Map.filter (a `elem`) $ graph

findSource :: Ord a => Graph a -> Maybe a
findSource graph = listToMaybe $ Map.foldl (\\) (Map.keys graph) graph

topologicalSort :: Ord a => Graph a -> Maybe [a]
topologicalSort graph | Map.null graph = Just []
topologicalSort graph = case findSource graph of
  Nothing -> trace "oof there a cycle oh nooooo" Nothing
  Just s -> (s:) <$> topologicalSort (subgraph (/= s) graph)

--------------------------------------------------------------------------------

mergeBidirectional :: Ord a => Graph a -> Graph [a]
mergeBidirectional = merge . listize
  where listize = fmap (fmap (:[])) . Map.mapKeys (:[])
        merge :: Ord a => Graph [a] -> Graph [a]
        merge graph = undefined

iffGroups :: Ord l => [l] -> [[l]] -> [[(l,l)]]
iffGroups outcomes prefs =
  let (_, freePairs) = separateDominatedPairs outcomes prefs

--------------------------------------------------------------------------------

-- Goal: one day this function gives a full combinatorial classification of
-- two-d prefs. I.e. it turns it into a discrete,compact representation of P2(X),
-- or fails iff the pref set is not two dimensional.
fullThing :: Ord l => [l] -> [[l]] -> Maybe ([(l,l)], [(l,l)])
fullThing outcomes prefs = (fixedPairs,) <$> freePairList
  where (fixedPairs, freePairs) = separateDominatedPairs outcomes prefs
        implicationGraph = pairImplicationGraph outcomes prefs
        freePairList = case freePairs of
          [] -> Just []
          (p:_) -> topologicalSort . componentOf p $ implicationGraph
-- Current problem: there can be two-node cycles in the pair-implication graph.
--   This is fine, but I don't know how to ``break the tie''.
--   I think it's like the implication graph is ``underspecified''.

--------------------------------------------------------------------------------

gShow :: Show i => Graph i -> IO ()
gShow = mapM_ print . Map.toList

prefs01 = [ [2,4,1,3], [2,3,4,1], [2,4,3,1], [3,2,4,1] ]

gr = pairImplicationGraph [1..4] prefs02 -- componentOf (1,3) $

prefs02 = [ [2,1,4,3], [2,4,1,3], [4,2,3,1] ]

prefs03 = [ [1,2,4,3], [2,4,1,3], [1,3,4,2], [1,3,2,4] ]

prefs04 = [ [1,2,3], [3,2,1] ]
