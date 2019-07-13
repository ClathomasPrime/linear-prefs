module MaxNormPeakPit where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Maybe

import Util
-- import LinearPref
-- import Condorcet
import ValRestr

-- import Debug.Trace

primitiveRelations :: Ord a => VRSystem a -> [( (a,a), (a,a) )]
primitiveRelations sys = nub $ sys >>= genRels
  where genRels (VRWorstRestr, j, i,k)
          | i <= j && j <= k = [ ((i,j), (i,k)), ((i,k), (j,k))]
        genRels (VRBestRestr, j, i,k)
          | i <= j && j <= k = [ ((j,k), (i,k)), ((i,k), (i,j))]
        genRels _ = error "don't feed non-normpeakpit domains here"

coveringRelations :: Ord a => [a] -> VRSystem a -> [( (a,a), (a,a) )]
coveringRelations outcomes sys
  = filter (not . impliedByOtherPair) primRels
  where primRels = primitiveRelations sys
        pairs = allPairs outcomes
        impliedByOtherPair (p1 , p2)
          = any (\p -> (p1, p) `elem` primRels && (p, p2) `elem` primRels) pairs


findSemireversed :: Ord a => [[a]] -> [[a]]
findSemireversed prefs = filter (\p -> any (semirev p) prefs) prefs
  where semirev p q = head p == last q && last p == head q

------ ------ ------ ------ ------ ------ ------ ------ ------ ------
concatProduct :: Ord a =>
  [a] -> VRSystem a -> [a] -> VRSystem a -> VRSystem a
concatProduct as sysA bs sysB = sysA ++ sysB ++ aab ++ abb
  where aab = [ (VRWorstRestr, j,i,k) | i:t <- tails as, j <- t, k <- bs]
        abb = [ (VRBestRestr, j,i,k) | i <- as, j:t <- tails bs, k <- t]

fishburns :: Int -> VRSystem Int
fishburns n =
  [ if even b then (VRWorstRestr,b,a,c) else (VRBestRestr,b,a,c)
    | (a,b,c) <- allTriples [1..n] ]

-- length $ maxPrefSet [1..12] $ fishburnsSquared 6
-- => 3573
-- fb(n) `simple grid product` fb(n)
fishburnsSquared :: Int -> VRSystem Int
fishburnsSquared n =
  concatProduct [1..n] (fishburns n) [n+1..2*n] (fmap phi $ fishburns n)
  where phi (c,i,j,k) = (c, n+i, n+j, n+k)

fishburnsSquaredSizeLb :: Integer -> Integer
fishburnsSquaredSizeLb n = (fishburnsSize n)^2 + (2*n `choose` n)

-- Best known lower bound for gamma_n (asymptotically) :
--   maximum . fmap (\n -> (fromIngetral . fishburnsSize $ n)**(1/n)) $ [1..100]
--   => (28, 2.0761796607487266)

fishburnsSize :: Integer -> Integer
fishburnsSize n | even n
  = 2^(n-3)*(n+3)
    - (((n-2) `choose` (n`div`2 - 1))`div` 2) * (2*n - 3)
fishburnsSize n | otherwise
  = 2^(n-3)*(n+3)
    - ((n-1) `choose` ((n-1)`div`2)) * ((n - 1)`div`2)

-- for n = 2^ex
recursiveGrid :: Int -> VRSystem Int
recursiveGrid ex = fmap (phi ex 0) (allTriples [(1::Int)..2^ex])
  where phi 0 _ _ = error "shouldn't go down to zero with distinct triples"
        phi e offset (i,j,k) -- i < j < k
          | i <= half && j <= half && k <= half
            = phi (e-1) offset (i,j,k)
          | i > half && j > half && k > half
            = phi (e-1) half (i,j,k)
          | i <= half && j <= half && k > half
            = (VRWorstRestr, j, i,k)
          | i <= half && j > half && k > half
            = (VRBestRestr, j, i,k)
          | otherwise = error "should be impossible if i < j < k"
          where half = 2^(e-1) + offset

-- right now this is only a lower bound. For n = 2^k
recursiveGridSize :: Int -> Integer
recursiveGridSize 1 = 2
recursiveGridSize k =
  (recursiveGridSize (k-1))^(2::Int) + ((2^k) `choose` (2^(k-1))) - 1

------ ------ ------ ------ ------ ------ ------ ------ ------ ------

type ChainList a = Map a [a]

chainListToVrSystem :: (Enum a, Ord a) => a -> a -> ChainList a -> VRSystem a
chainListToVrSystem minA maxA chains
  = concatMap snd . Map.toList . Map.mapWithKey calcForJ $ chains
  where calcForJ j chain
          = [ (calc i k chain, j, i,k) | i <- [minA..pred j], k <- [succ j..maxA] ]
        calc i k chain =
          case (<) <$> elemIndex i chain <*> elemIndex k chain of
            Just True -> VRWorstRestr -- i before k in chain
            Just False -> VRBestRestr -- k before i in chain
            Nothing -> error "look out! Should put everything in your chains dog"

triAltScheme :: Int -> VRSystem Int
triAltScheme n =
  [ if b `mod` 3 == 0 then (VRWorstRestr,b,a,c) else (VRBestRestr,b,a,c)
    | (a,b,c) <- allTriples [1..n] ]

dipDiveDodge :: VRSystem Int
dipDiveDodge = chainListToVrSystem 1 10 $ Map.fromList . zip [1..] $
  [ [7,8,9,5,10,6,4,3,2]
  , [7,8,5,9,6,10,4,3,1]
  , [7,5,8,6,9,10,4,2,1]
  , [5,7,6,8,9,10,3,2,1]

  , [4,7,3,8,2,9,1,10,6]
  , [7,4,8,3,9,2,10,1,5]

  , [6,4,5,3,2,1,8,9,10]
  , [4,6,3,5,2,1,7,9,10]
  , [4,3,6,2,5,1,7,8,10]
  , [4,3,2,6,1,5,7,8,9]
  ]

-- has 3490 prefs
weaveFishbRec :: VRSystem Int
weaveFishbRec
  =  [ if even b then (VRWorstRestr,b,a,c) else (VRBestRestr,b,a,c)
      | (a,b,c) <- allTriples [1..4] ]
  ++ [ if even b then (VRWorstRestr,b,a,c) else (VRBestRestr,b,a,c)
      | (a,b,c) <- allTriples [5..8] ]
  ++ [ if even b then (VRWorstRestr,b,a,c) else (VRBestRestr,b,a,c)
      | (a,b,c) <- allTriples [9..12] ]
  ++ [ (VRWorstRestr,b,a,c)
      | (a,b) <- allPairs [1..4], c <- [5..12] ]
  ++ [ (VRBestRestr,b,a,c)
      | a <- [1..8], (b,c) <- allPairs [9..12] ]

  ++ [ (VRBestRestr, b,a,c)
      | a <- [1..4], (b,c) <- allPairs [5..8] ]
  ++ [ (VRWorstRestr, b,a,c)
      | (a,b) <- allPairs [5..8], c <- [9..12] ]

  ++ [ if b == 7 || b == 8 then (VRWorstRestr,b,a,c) else (VRBestRestr,b,a,c)
      | a <- [1..4], b <- [5..8], c <- [9..12] ]


-- dipDiveDodge :: VRSystem Int
-- dipDiveDodge
--   =  [ (VRWorstRestr, 5, i,k) | (i,k) <- [(4,7),(3,8),(2,9),(1,10)] ]
--   ++ [ (VRBestRestr, 6, i,k) | (i,k) <- [(4,7),(3,8),(2,9),(1,10)] ]
--   ++ [ (VRBestRestr, j, i,k) | (i,j,k) <- allTriples [1..4] ]
