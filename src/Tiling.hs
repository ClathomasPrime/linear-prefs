{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards       #-}

module Tiling where

import Data.List
import Data.Maybe
import Data.Function
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
myExtrusion = [ [1], [1,2], [1,3,2] ] -- , [1,4,3,2] ]

trialtExtrusion :: [Snake Int]
trialtExtrusion = [ [1], [2,1] , [2,1,3] , [4,2,1,3] , [5,4,2,1,3] ]

alternating :: Int -> [Snake Int]
alternating i = unfoldr accum (1, [])
  where accum (j,l)
          | j < i =
            let l' = if even j then l ++ [j] else [j] ++ l
             in Just (l', (j+1,l'))
          | otherwise = Nothing

extrusionConcatProduct :: [Snake Int] -> [Snake Int] -> [Snake Int]
extrusionConcatProduct sn sn'
  = extrusionConcatProductFull [n,n-1..1] sn $ (fmap.fmap) (+n) sn'
  where n = length sn + 1

extrusionConcatProductFull :: [a] -> [Snake a] -> [Snake a] -> [Snake a]
extrusionConcatProductFull as sn sn' = sn ++ fmap (as++) ([]:sn')

----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- Building the tiling domain
----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

-- for each outcome's extrusion sequence, list the (unordered) prefixes of the
-- extrusion sequence, then what comes next (Nothing if its the whole sequence)
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


-- If handed an actual vertex (identified by its unordered prefix set, must be
-- assumed to be present in the actual tiling) it spits out the possible ``next
-- outcomes'' which might come in the domain.
neighborSuccessors :: (Show a, Enum a, Ord a) => TilingDomain a -> Vertex a -> Set a
neighborSuccessors TilingDomain{..} vertex =
  let previousVertex next =
        vertex `S.intersection` S.fromList [tdMinOutcome..pred next]
        -- the vert which vertex corresponded to BEFORE next was extruded
      accumulator successors (next, prefixNexts) =
        -- prefixNexts is basically the snake which next was extruded along.
        -- trace ("hi " ++ show next) $
        case M.lookup (previousVertex next) prefixNexts of
          Nothing -> successors
            -- ^ vertex not along snake ``next'' was extruded along.
          Just nextInTrack ->
            let successors' = case nextInTrack of
                  Nothing -> successors
                    -- ^ prevVert was at the top of the track getting extruded
                  Just trackSplit -> -- trace ("ts " ++ show trackSplit) $
                    -- ^ prevVert was followed up by trackSplit along the extrusion
                    if next `S.member` vertex
                       then S.filter (>= trackSplit) successors
                         -- keep things that come after trackSplit
                       else S.filter (<= trackSplit) successors
                         -- keep things that come before trackSplit
             in if next `S.member` vertex
                   then successors' -- ^ next notIn vertex AND vertex not a prefix
                   else S.insert next successors'
   in foldl' accumulator S.empty . M.toAscList $ tdPrefixNexts

buildTilingDomain :: forall a. (Enum a, Ord a, Show a)
  => a -> a -> [Snake a] -> TilingDomain a
buildTilingDomain minA maxA extrudedSnakes = dom1 { tdVertCovers = vertCovers }
  where dom1@TilingDomain{..} = buildPrefixNexts minA maxA extrudedSnakes
        vertCovers :: Map (Vertex a) (Set a)
        vertCovers = snd $ until (S.null . fst) accum (S.singleton S.empty, M.empty)
        accum (frontier, m) | S.null frontier = (frontier, m)
        accum (frontier, m) =
          let (vert, frontier') = S.deleteFindMin frontier
              nexts = neighborSuccessors dom1 vert
              frontier'' = frontier' `S.union`
                S.map (\n -> S.insert n vert) nexts
           in (frontier'', M.insert vert nexts m)


----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- Navigating the tiling domain
----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

-- assumes vert actually present in graph
successorVerts :: Ord a => TilingDomain a -> Vertex a -> Set (Vertex a)
successorVerts TilingDomain{..} vert
  = S.map (`S.insert` vert) . fromJust . M.lookup vert $ tdVertCovers

----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- Statistics
----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

countFromSnakes :: Integral i => [Snake Int] -> i
countFromSnakes snakes =
  countTilingDomain $ buildTilingDomain 1 (length snakes + 1) snakes

countTilingDomain :: (Integral i, Enum a, Ord a) => TilingDomain a -> i
countTilingDomain dom@TilingDomain{..}
  = fromJust . M.lookup (S.fromList [tdMinOutcome..tdMaxOutcome]) $ vertPrefCounts
  where vertPrefCounts = foldl accum initCounts increasingSizeVerts
        initCounts = M.insert S.empty 1 (fmap (const 0) tdVertCovers)
        increasingSizeVerts = sortBy (compare `on` S.size) (M.keys tdVertCovers)

        accum counts vert =
          let currentCount = fromJust . M.lookup vert $ counts
              nexts = successorVerts dom vert
           in S.foldl (\m n -> M.adjust (+currentCount) n m) counts nexts
  -- where vertPrefCounts = (\(_,_,r) -> r) result
  --       result = until (\(f1,f2,_) -> S.null . fst) accum
  --         (S.singleton S.empty, S.empty, M.empty)
  --       accum (frontCurrent, frontNext, m)
  --         | S.null frontCurrent = (frontNext, S.empty, m)
  --       accum (frontCurrent, frontNext, m) =
  --         let (vert, frontier') = S.deleteFindMin frontier
  --             nexts = undefined
  --             frontier'' = frontier' `S.union`
  --               S.map (\n -> S.insert n vert) nexts
  --          in (frontier'', M.insert vert nexts m)

----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

weaveFishbRecSn :: [Snake Int]
weaveFishbRecSn =
  [ [1]
  , [1,2]
  , [3,1,2]

  , [1,2,3,4]
  , [5, 1,2,3,4]
  , [5,6, 1,2,3,4]
  , [7,5,6, 1,2,3,4]

  , [5,6, 1,2,3,4, 7,8]
  , [9, 5,6, 1,2,3,4, 7,8]
  , [9,10, 5,6, 1,2,3,4, 7,8]
  , [11,9,10, 5,6, 1,2,3,4, 7,8]
  ]

-- 2915
splitRec :: [Snake Int]
splitRec =
  {-2 -} [ [1]
  {-3 -} , [1,2]
  {-4 -} , [3,1,2]
  {-5 -} , [3,1,2,4]
  {-6 -} , [5,3,1,2,4]
  {-7 -} , [5,3,6,1,4,2]
  {-8 -} , [7,5,3,6,1,4,2]
  {-9 -} , [5,3,6,1,4,2, 7,8]
  {-10-} , [9,7, 5,3,6,1,4,2, 8]
  {-11-} , [5,3,6,1,4,2, 9,7,8,10]
  {-12-} , [11,9, 5,3,6,1,4,2, 7,10,8]
  ]

-- 579
upperDowner :: [Snake Int]
upperDowner =
  {-2 -} [ [1]
  {-3 -} , [2,1]
  {-4 -} , [2,3,1]
  {-5 -} , [4, 2,3,1]
  {-6 -} , [4,5, 3,2,1]
  {-7 -} , [6,4,5, 3,2,1]
  {-8 -} , [4,5, 3,2, 1, 6,7]
  {-9 -} , [4,5, 3,2,8, 1,6,7]
  {-10-} , [4,5, 2,3, 1, 6,7,8,9]
  ]

dipDiveDodgeOg :: [Snake Int]
dipDiveDodgeOg =
  {-2 -} [ [1]
  {-3 -} , [2,1]
  {-4 -} , [3,2,1]

  -- {-5 -} , [4,7, 3,8, 6, 9,2, 10,1]
  {-5 -} , [4,3,2,1]
  {-6 -} , [4,3,  2,1 ,5]

  {-7 -} , [6,4,5, 3,2,1]
  {-8 -} , [4, 6,3,5, 2,1,7]
  {-9 -} , [4,3, 6,2,5, 1,7,8]
  {-10-} , [4,3,2, 6,1,5, 7,8,9]
  ]
