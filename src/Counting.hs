module Counting where

import Data.List
import Util

-- -- -- -- -- --
-- Random stuff
-- -- -- -- -- --

myarbrec :: Int -> Int
myarbrec 2 = 2
myarbrec 3 = 4
myarbrec n = myarbrec (n-1) + n * myarbrec (n-2)

myNumFunc :: Int -> Int -> Int
myNumFunc n w = ((n-w) `c` 3) + ((n-w) `c` 1) * (w `c` 2)
  - ((n-w) `c` 2) * (w `c` 1) - (w `c` 3)
  where c m k
          | k > m || k < 0 = 1
          | otherwise = m `choose` k

cylinderGrid :: Integral i => i -> i
cylinderGrid n =
  (n + 3) * ((2*n - 1) `choose` n)
  - 2^(2*n - 1)

gridPaths :: Integral i => i -> i -> i
gridPaths a b = (a + b - 2) `choose` (b -1)

badPaths :: Integral i => i -> i -> i -> i
badPaths _ _ 0 = 0
badPaths _ _ 1 = 1
badPaths a b u = sum . fmap term $ [0..u-1]
  where term i =
          let topH = a - u + i
              topW = i+1
              botH = u - i
              botW = b - i
           in (gridPaths topH topW - badPaths topH topW i) * gridPaths botH botW

badPathsGuess :: Integral i => i -> i -> i -> i
badPathsGuess a b u = (a+b - 2) `choose` (u - 1)

cylinderGridSum :: Integral i => i -> i
cylinderGridSum n = sum . fmap term $ series
  where series = [ (l,r) | l <- [0..n], r <- [max (l-1) 0 .. min n (n-1+l) ] ]
        term (l,r) =
          let height = n + l - r
              width = n - 1
              badLow = max (n - 1 - r) 0
              badHigh = max (l - 1) 0
           in gridPaths height width - badPaths height width badLow
                - badPaths height width badHigh

nonIncrSeqs :: (Integral i) => i -> i -> [[i]]
nonIncrSeqs 0 _ = [[]]
nonIncrSeqs b a = [ i0:rest | i0 <- [1..a], rest <- nonIncrSeqs (b-1) i0 ]

goodPaths :: Integral i => i -> i -> i -> i -> i
goodPaths a b u v = fromIntegral . length . filter good $ paths
  where paths = nonIncrSeqs (b-1) a
        good path = all (\(i,j) -> check i j) $ zip [2..b] path
        check i j
          | lowerBlue (i-1) j = False
          | upperBlue i j = False
          | otherwise = True
        lowerBlue i j = i + j <= u + 1
        upperBlue i j = (b-i+1) + (a-j+1) <= v + 1

doubleBadPaths :: Integral i => i -> i -> i -> i -> i
doubleBadPaths a b u v = fromIntegral . length . filter doubleBad $ paths
  where paths = nonIncrSeqs (b-1) a
        doubleBad path =
          let path' = zip [2..b] path
           in any (uncurry upperBlue) path' && any (uncurry leftLowerBlue) path'
        leftLowerBlue i j = (i-1) + j <= u + 1
        upperBlue i j = (b-i+1) + (a-j+1) <= v + 1

doubleBadPathsUpFirst :: Integral i => i -> i -> i -> i -> i
doubleBadPathsUpFirst a b u v = fromIntegral . length . filter doubleBad $ paths
  where paths = nonIncrSeqs (b-1) a
        doubleBad path =
          let path' = zip [2..b] path
           in case (findIndex upperBlue path', findIndex leftLowerBlue path') of
                (Just i, Just i') -> i <= i'
                _ -> False

        leftLowerBlue (i,j) = (i-1) + j <= u + 1
        upperBlue (i,j) = (b-i+1) + (a-j+1) <= v + 1

-- # of monotone lattice paths (0,0) to (a,b) (so a+1 by b+1 vertices)
-- avoiding the lines y=x+s and y=x-t
-- (so b-s+1 bad nodes along the b side
-- and a-t+1 bad notes along the a side)
lineAvoidingPath :: Integral i => i -> i -> i -> i -> i
lineAvoidingPath a b s t = sum . fmap term $ [kMin..kMax]
  where kMin = negate $ b `div` (s + t) + 1
        kMax = (a - t) `div` (s + t) + 1
        term k = ( (a+b) `choose` (b + k * (s+t)) )
          - ( (a+b) `choose` (b + k * (s+t) + t) )

sumsumpath :: Integral i => i -> i
sumsumpath n = sum . fmap lineAvoidingPath' $ [0..n]
  where lineAvoidingPath' l =
          ((2*n-1)`choose` n) - ((2*n-1)`choose`(n+l+1))
            - ((2*n-1)`choose`(l-2))

simplif :: Integral i => i -> i
simplif n = (n+1) * ( (2*n - 1) `choose` n ) - 2^(2*n - 1)
        -- term l =
        --   let a = n - 1
        --       b = n
        --       s = n - l + 2
        --       t = l + 1
        --    in lineAvoidingPath a b s t

-- termywormy :: Integral i => i -> i
-- termywormy n =
--   let a = n
--       b = n
--       s = 2
--       t = n + 1
--    in lineAvoidingPath a b s t

exper n m = mapM_ print $ (fmap (\(i, j) ->
  (doubleBadPathsUpFirst n m i j) )
  -- (doubleBadPaths n m i j,
  --   (doubleBadPathsUpFirst n m i j) + (doubleBadPathsUpFirst m n j i)))
  <$> [ [(i,j) | j <- [1..max n m -1] ] | i <- [1..max m n - 1:: Int] ])


