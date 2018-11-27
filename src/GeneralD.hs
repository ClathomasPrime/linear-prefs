{-# LANGUAGE ScopedTypeVariables       #-}

module GeneralD where

import Control.Monad.Random
import Data.List
import Data.Function
import Data.Ratio

import Util
import LinearPref

type Point a = [a]

unitBoxPoints :: MonadRandom m => Int -> Int -> m [Point Double]
unitBoxPoints d n = replicateM n point
  where point = replicateM d (getRandomR (0.0, 1.0))

roundedUnitBoxPoints :: (Fractional n, Enum n, Ord n, MonadRandom m)
  => Int -> Int -> Int -> m [Point n]
roundedUnitBoxPoints prec d n = replicateM n point
  where point = replicateM d coord
        coord = do
          i :: Int <- getRandomR (0, 10^prec)
          return $ fromIntegral i / fromIntegral (10^prec :: Int)

--------------------------------------------------------------------------------

type PrefWeight a = [a]

dot :: Num a => PrefWeight a -> Point a -> a
dot a x = sum $ zipWith (*) a x

linearPref :: (Ord a, Num a, Ord l) => PrefWeight a -> [(l, Point a)] -> [l]
linearPref a = fmap fst . sortBy cmp
  where cmp (lx,x) (ly,y) =
          (a `dot` y) `compare` (a `dot` x) <> ly `compare` lx
          -- ^ oposite order to sort biggest to smallest

--------------------------------------------------------------------------------

meshNSphere :: (Floating a, Enum a) => Int -> Int -> [[a]]
meshNSphere d k = fmap sphereFromCoords coords
  where coords = nProd (d-1) [0.0, pi/2/k'.. pi/2]
        k' = fromIntegral k

sphereFromCoords :: Floating a => [a] -> [a]
sphereFromCoords [] = [1]
sphereFromCoords (t:ts) = cos t : fmap (* sin t) (sphereFromCoords ts)

meshNCell :: (Fractional a, Enum a) => Int -> a -> [[a]]
meshNCell dim delta = meshNCell' dim 1.0
  where meshNCell' 1 total = [[total]]
        meshNCell' n total = do
          x <- [0,delta..total]
          ys <- meshNCell' (n-1) (total - x)
          return $ x:ys

--------------------------------------------------------------------------------

genLinearPrefs :: (Fractional n, Enum n, Ord n, Ord a)
               => Int -> n -> [(a, Point n)] -> [[a]]
genLinearPrefs d delta xs
  = nub [linearPref a xs | a <- meshNCell d delta]

mostPrefs :: (Fractional n, Enum n, Ord n, MonadRandom m)
          => Int -> Int -> Int -> n -> Int
          -> m (Int, [Point n])
mostPrefs n d trials delta prec
  = record numPrefs <$> bestPoints
  where bestPoints = most cmp trials (roundedUnitBoxPoints prec d n)
        numPrefs = length . genLinearPrefs d delta . arbitraryLable
        cmp = compare `on` numPrefs

--------------------------------------------------------------------------------

experiment :: MonadRandom m => m [(Int,Int)]
experiment = mapM phi [4..6]
  where phi n = do
          (len,_) <- mostPrefs n 3 100 (0.003 :: Rational) 2
          return (n,len)

points01 :: [[Double]]
points01 =
  [ [0.225,0.987]
  , [0.528,0.661]
  , [0.421,0.668]
  , [0.425,0.467]
  , [3.0e-2,0.775]
  , [0.957,0.368]
  , [0.897,2.0e-2]
  , [0.208,0.55]
  , [0.755,0.537]
  , [0.755,1.4e-2]
  ]

points02 :: [[Rational]]
points02 =
  [[0.93,0.1],[0.63,0.39],[0.3,0.81],[0.98,0.45],[0.57,0.79],[0.38,0.98],[0.39,0.88],[0.87,0.3],[0.76,0.32],[0.0,0.55]]
