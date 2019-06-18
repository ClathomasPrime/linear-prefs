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
searchGenSc :: forall l. (Ord l, Show l) => [l] -> [[l]] -> Maybe (Tree (l,l))
searchGenSc root domain
  = case searchGenSc' root domain of
      (t, []) -> Just t
      _ -> Nothing

searchGenSc' :: forall l. (Ord l, Show l) => [l] -> [[l]] -> (Tree (l,l), [(l,l)])
searchGenSc' root domain =
  let graph = condorcetGraphEdges' domain
      srt (u,v)
        | u <= v = (u,v)
        | otherwise = (v,u)
      searchGenScRecur :: [(l,l)] -> [l] -> (Tree (l,l), [(l,l)])
      searchGenScRecur remainingPairs current =
        let adjFlipsLeft = filter ((`elem` remainingPairs) . srt . snd)
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
   in searchGenScRecur (fmap srt $ allPairs root) root

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
