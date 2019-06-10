module MaxNormPeakPit where

import Data.List
-- import Data.Maybe

import Util
-- import LinearPref
-- import Condorcet
import ValRestr

import Debug.Trace

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
fishburnsSquared :: Int -> VRSystem Int
fishburnsSquared n =
  concatProduct [1..n] (fishburns n) [n+1..2*n] (fmap phi $ fishburns n)
  where phi (c,i,j,k) = (c, n+i, n+j, n+k)

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
