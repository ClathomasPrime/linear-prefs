{-# LANGUAGE TupleSections #-}
module ConnectedGraph where

import Util
import ConvertTwoD
-- import GeneralD
import LinearPref
-- import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Tuple
-- import Data.Function
import Data.Maybe

import Debug.Trace


--------------------------------------------------------------------------------
-- Graph stuff (most of the rest of this stuff) is outdated: I was overthinking it

type Graph x = Map x [x]

pairImplicationGraph :: Ord l => [l] -> [[l]] -> Graph (l,l)
pairImplicationGraph outcomes prefs =
  let (_, freePairs) = separateDominatedPairs prefs
      -- conclusionsFrom p = intersect (conclusionsThatHold [p] prefs) freePairs \\ [p]
      conclusionsFrom p = filter (checkImplies prefs p) freePairs
   in Map.fromList [ (p, conclusionsFrom p) | p <- freePairs ]

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

oldFullThing :: Ord l => [l] -> [[l]] -> Maybe ([(l,l)], [(l,l)])
oldFullThing outcomes prefs = (fixedPairs,) <$> freePairList
  where (fixedPairs, freePairs) = separateDominatedPairs prefs
        implicationGraph = pairImplicationGraph outcomes prefs
        freePairList = case freePairs of
          [] -> Just []
          (p:_) -> topologicalSort . componentOf p $ implicationGraph
-- Current problem: there can be two-node cycles in the pair-implication graph.
--   This is fine, but I don't know how to ``break the tie''.
--   I think it's like the implication graph is ``underspecified''.

--------------------------------------------------------------------------------

-- gShow :: Show i => Graph i -> IO ()
-- gShow = mapM_ print . Map.toList
