{-# LANGUAGE ScopedTypeVariables
           , TupleSections
  #-}
module Abnormal where

import Data.Maybe
import Data.List
import Data.Function

import Debug.Trace

import Util
import Condorcet
import SingleCrossing

-- tree with labels everywhere except the root
type Tree a = [Branch a]

data Branch a = Branch
  { branchRoot :: a
  , branchChildren :: Tree a
  } deriving(Show, Eq, Ord)

-- depth-first (sorta - it adds as much as it can at the current level then recurses
--   to the children in order)
--   add pairs to the first match that you come across.
-- Might be unreasonable if you feed in a non-connected domain.
fanSearchGenSc :: forall l. (Ord l, Show l) => [l] -> [[l]] -> Maybe (Tree (l,l))
fanSearchGenSc root domain
  = case fanSearchGenSc' root domain of
      (t, []) -> Just t
      _ -> Nothing

fanSearchGenSc' :: forall l. (Ord l, Show l) => [l] -> [[l]] -> (Tree (l,l), [(l,l)])
fanSearchGenSc' root domain =
  let graph = condorcetGraphEdges' domain
      searchGenScRecur :: [(l,l)] -> [l] -> (Tree (l,l), [(l,l)])
      searchGenScRecur remainingPairs current =
        let adjFlipsLeft = filter ((`elem` remainingPairs) . sortPair . snd)
              . nubBy ((==) `on` snd) . catMaybes
              . fmap (\p -> (p,) <$> swapDifference current p)
              . neighbors current $ graph
            -- ^ (neighbor,swap) pairs not yet swapped
         in foldl iter ([], remainingPairs \\ fmap snd adjFlipsLeft) adjFlipsLeft
      iter :: ([Branch (l, l)], [(l, l)])
                   -> ([l], (l, l)) -> ([Branch (l, l)], [(l, l)])
      iter (accum, remainingPairs) (current, justFlipped) =
        let (recur, remainingPairs')
              = searchGenScRecur (remainingPairs \\ [justFlipped]) current
         in ((Branch justFlipped recur):accum, remainingPairs')
   in searchGenScRecur (fmap sortPair $ allPairs root) root

dfsSearchGenSc :: forall l. (Ord l, Show l) => [l] -> [[l]] -> Maybe (Tree (l,l))
dfsSearchGenSc root domain
  = case dfsSearchGenSc' root domain of
      (t, []) -> Just t
      _ -> Nothing

dfsSearchGenSc' :: forall l. (Show l, Ord l) => [l] -> [[l]] -> (Tree (l,l), [(l,l)])
dfsSearchGenSc' root domain =
  let graph = condorcetGraphEdges' domain
      searchGenScRecur :: [(l,l)] -> [l] -> (Tree (l,l), [(l,l)])
      searchGenScRecur remainingPairs current =
        let adjPrefFlips = nubBy ((==) `on` snd) . catMaybes
              . fmap (\p -> (p,) <$> swapDifference current p)
              . neighbors current $ graph
            -- ^ (neighbor,swap) pairs not yet swapped
         in foldl iter ([], remainingPairs) adjPrefFlips
      iter :: ([Branch (l, l)], [(l, l)])
                   -> ([l], (l, l)) -> ([Branch (l, l)], [(l, l)])
      iter (accum, remainingPairs) (current, justFlipped) =
        if justFlipped `elem` remainingPairs
          then let (recur, remainingPairs')
                     = searchGenScRecur (remainingPairs \\ [justFlipped]) current
                in ((Branch justFlipped recur):accum, remainingPairs')
          else (accum, remainingPairs)
   in searchGenScRecur (fmap sortPair $ allPairs root) root

--------------------------------------------------------------------------------

domainOfTree :: Ord l => [l] -> Tree (l,l) -> [[l]]
domainOfTree root tree = root : rest
  where rest = do
          Branch (u,v) children <- tree
          let next = flipPair (u,v) root
          domainOfTree next children


-- these are all that come out of the (first draft of the) tree-finding
-- algorithm, run on `abnormalPeakPits5`
someTreesOnFive :: [Tree (Int,Int)]
someTreesOnFive =
  [ -- completion size: 12
    [Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = [Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,4), branchChildren = [Branch {branchRoot = (1,2), branchChildren = []}]}]}]},Branch {branchRoot = (3,4), branchChildren = []}]}]}]}]}]}]
  , -- completion size: 12
    [Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = [Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,4), branchChildren = [Branch {branchRoot = (1,2), branchChildren = []}]}]}]}]}]}]},Branch {branchRoot = (3,4), branchChildren = []}]}]}]
  , -- completion size: 15
    [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = []}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = []},Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (1,4), branchChildren = [Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]}]}]}]}]
  , -- completion size: 14
    [Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]},Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (1,4), branchChildren = [Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = []}]}]}]}]}]}]
  , -- completion size: 13
    [Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = []}]}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = []}]},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,4), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]}]}]
  , -- completion size: 14
    [Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (3,5), branchChildren = []}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = []}]}]},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,4), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]}]}]
  , -- completion size: 17
    [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (3,4), branchChildren = []},Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = []},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]}]}]
  , -- completion size: 15
    [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]},Branch {branchRoot = (3,4), branchChildren = []},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = []},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]}]}]
  , -- completion size: 12
    [Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = []},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]}]}]
  , -- completion size: 14
    [Branch {branchRoot = (3,4), branchChildren = []},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]}]},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]}]}]
  , -- completion size: 15
    [Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []},Branch {branchRoot = (3,4), branchChildren = []}]}]}]}]},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]}]}]
  , -- completion size: 14
    [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]},Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = []}]}]}]
  , -- completion size: 13
    [Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]},Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = []}]}]}]
  , -- completion size: 16
    [Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]},Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]},Branch {branchRoot = (1,3), branchChildren = [Branch {branchRoot = (1,2), branchChildren = []}]}]}]
  , -- completion size: 16
    [Branch {branchRoot = (4,5), branchChildren = [Branch {branchRoot = (3,5), branchChildren = [Branch {branchRoot = (2,5), branchChildren = [Branch {branchRoot = (1,5), branchChildren = []}]}]}]},Branch {branchRoot = (3,4), branchChildren = [Branch {branchRoot = (2,4), branchChildren = [Branch {branchRoot = (1,4), branchChildren = []}]}]},Branch {branchRoot = (2,3), branchChildren = [Branch {branchRoot = (1,3), branchChildren = []}]},Branch {branchRoot = (1,2), branchChildren = []}]
  ]

-- starts at [1..5]
oneMaxTreeOnFive :: Tree (Int,Int)
oneMaxTreeOnFive =
  [ Branch (2,3)
  [ Branch (2,4)
  [ Branch (1,3)
  [ Branch (2,5)
    [ Branch (4,5) []
    , Branch (1,4)
      [ Branch (3,4)
      [ Branch (1,5)
        [ Branch (3,5) []
        , Branch (1,2) []
        ] ] ] ] ] ] ] ]
-- unique maximal extension => head maximumAbnormalPeakPitFive
