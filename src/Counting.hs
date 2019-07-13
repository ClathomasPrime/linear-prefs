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

exper n m = mapM_ print $ (fmap (\(i, j) ->
  (doubleBadPathsUpFirst n m i j) )
  -- (doubleBadPaths n m i j,
  --   (doubleBadPathsUpFirst n m i j) + (doubleBadPathsUpFirst m n j i)))
  <$> [ [(i,j) | j <- [1..max n m -1] ] | i <- [1..max m n - 1:: Int] ])
