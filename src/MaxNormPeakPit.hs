module MaxNormPeakPit where

import Data.List
-- import Data.Maybe

import Util
-- import LinearPref
-- import Condorcet
import ValRestr

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

fishburnsSize :: Int -> Int
fishburnsSize n | even n
  = 2^(n-3)*(n+3)
    - (((n-2) `choose` (n`div`2 - 1))`div` 2) * (2*n - 3)
fishburnsSize n | otherwise
  = 2^(n-3)*(n+3)
    - ((n-1) `choose` ((n-1)`div`2)) * ((n - 1)`div`2)

