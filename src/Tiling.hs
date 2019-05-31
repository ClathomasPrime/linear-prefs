{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards       #-}

module Tiling where

import Data.List
-- import Data.Function
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)

-- import Util

import Debug.Trace

-- Confusing thing is file uses: RecordWildCards


type Snake a = [a]

-- Vertex is located by its (unordered) prefix
type Vertex a = Set a

data TilingDomain a = TilingDomain
  { tdMinOutcome :: a
  , tdMaxOutcome :: a
  , tdExtrudedSnakes :: Map a (Snake a)
  , tdPrefixNexts :: Map a (Map (Set a) (Maybe a))
  , tdVertCovers :: Map (Vertex a) (Set a)
  } deriving(Eq, Ord, Show)

myExtrusion :: [Snake Int]
myExtrusion = [ [1], [1,2], [1,3,2], [1,4,3,2] ]

buildPrefixNexts :: (Enum a, Ord a) => a -> a -> [Snake a] -> TilingDomain a
buildPrefixNexts minA maxA extrudedSnakes
  = TilingDomain minA maxA zipedSnakes prefixNexts M.empty
  where zipedSnakes = M.fromList $ zip [minA..maxA] $ []:extrudedSnakes
        prefixNexts = fmap prefixer zipedSnakes
        prefixer snake =
          let prefixes = init . tails . reverse $ snake
              tuples = fmap prefixNextTuple prefixes
              lastTuple = (S.fromList snake, Nothing)
           in M.fromList $ lastTuple:tuples
        prefixNextTuple (a:as) = (S.fromList as, Just a)
        prefixNextTuple _ = error "sacre bleu~"
-- Nothing - means not a prefix
-- Just Nothing - means [a] == set as a set
-- Just (Just b) - means its a prefix and b comes nex
--

neighborSuccessors :: (Show a, Enum a, Ord a) => TilingDomain a -> Vertex a -> Set a
neighborSuccessors TilingDomain{..} vertex =
  let previousVertex next = vertex `S.intersection` S.fromList [tdMinOutcome..pred next]
      accumulator successors (next, prefixNexts) =
        trace ("hi " ++ show next) $ case M.lookup (previousVertex next) prefixNexts of
          Nothing -> trace "notSucc" successors
          Just nextInTrack ->
            let successors' = case nextInTrack of
                  Nothing -> successors
                  Just trackSplit ->
                    if next `S.member` vertex
                       then S.filter (>= trackSplit) successors
                       else S.filter (<= trackSplit) successors
             in traceShowId $ if next `S.member` vertex
                   then successors' -- ^ next notIn vertex AND vertex not a prefix
                   else S.insert next successors'
   in foldl' accumulator S.empty . M.toAscList $ tdPrefixNexts

-- buildBasis :: Ord a => [a] -> [Snake a] -> TilingDomain a
-- buildBasis outcomes tracks = TilingDomain (S.fromList outcomes) tracks basis
--   where basis = M.fromList $ zip outcomes (fmap _ tracks)
--         -- phi a snake =
--
-- neighborSuccessors :: Ord a => TilingDomain a -> Vertex a -> Set a
-- neighborSuccessors dom prefix =
--   let nexts = tdOutcomes dom S.\\ prefix
--    in S.filter (isNeighbor dom prefix) nexts
--
-- isNeighbor :: Ord a => TilingDomain a -> Vertex a -> a -> Bool
-- isNeighbor = undefined
