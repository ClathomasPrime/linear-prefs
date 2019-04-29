{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections       #-}

module GeneralD where

import Control.Monad.Random
import Data.List
import Data.Function
-- import Data.Ratio

import Util
import LinearPref
import PrefDomains

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

unitCirclePrefs :: (MonadRandom m) => Int -> m [Point Double]
unitCirclePrefs n = replicateM n point
  where point = do
          t <- getRandomR (0, pi / 2)
          return [cos t, sin t]

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

randomMeshPoint :: (MonadRandom m, Fractional a, Enum a)
                => Int -> m (Point a)
randomMeshPoint d = do
  u <- take d . fmap (/10^3) . fmap fromIntegral <$> getRandomRs (0 :: Int,10^3)
  return $ fmap (/sum u) u

--------------------------------------------------------------------------------

genLinearPrefs :: (Fractional n, Enum n, Ord n, Ord a)
               => Int -> n -> [(a, Point n)] -> [[a]]
genLinearPrefs d delta xs
  = nub [linearPref a xs | a <- meshNCell d delta]

-- Would be really nice to finish this...
-- genLinearPrefsTieless :: (Fractional n, Enum n, Ord n, Ord a)
--   => Int -> n -> [(a, Point n)] -> [[a]]
-- genLinearPrefsTieless d delta xs
--   = nub . catMaybes $ [linearPref' a xs | a <- meshNCell d delta]
--   where linearPref' a xs =
--           if any
--         cmp (lx,x) (ly,y) = if (a `dot` y) `compare` (a `dot` x) <> ly `compare` lx
--                 -- ^ oposite order to sort biggest to smallest

mostPrefs :: (Fractional n, Enum n, Ord n, MonadRandom m)
          => Int -> Int -> Int -> n -> Int
          -> m ([Point n], Int)
mostPrefs n d trials delta prec
  = record numPrefs <$> bestPoints
  where bestPoints = most cmp trials (sort <$> roundedUnitBoxPoints prec d n)
        numPrefs = length . genLinearPrefs d delta . arbitraryLable
        cmp = compare `on` numPrefs

--------------------------------------------------------------------------------

randomFullPrefs :: MonadRandom m => Int -> Int -> Double -> m [[Int]]
randomFullPrefs d n delta
  = genLinearPrefs d delta . arbitraryLable <$> unitBoxPoints d n

simpleRandPrefs :: MonadRandom m => m [[Int]]
simpleRandPrefs = randPrefs 3 4 1000

-- all these are distinct
randPrefs :: MonadRandom m => Int -> Int -> Int -> m [[Int]]
randPrefs d numOutcomes numVoters = do
  outcomes <- arbitraryLable <$> roundedUnitBoxPoints 3 d numOutcomes
  voters <- replicateM numVoters (randomMeshPoint d)
  return . nub $ fmap (\(a :: Point Rational) -> linearPref a outcomes) voters

--------------------------------------------------------------------------------

experiment :: MonadRandom m => m [(Int,Int)]
experiment = mapM phi [4..6]
  where phi n = do
          (_,len) <- mostPrefs n 3 100 (0.003 :: Rational) 2
          return (n,len)

findexperiment :: MonadRandom m => m [[[Rational]]]
findexperiment = filter (hasThing . prefs) <$> things
  where
    -- hasThing = hasKCycle [1..n] k
    -- hasThing = hasPattern [[1,2,3],[2,1,3],[3,2,1],[3,1,2 :: Int]]
    -- hasThing = hasPattern noOneFirst
    hasThing = hasPattern (flipFlop \\ [[2,1,4,3]])
    -- hasThing = hasPattern (filter ((/=3) . head) $ fullSinglePeaked [1..4])
    prefs = genLinearPrefs d delta . arbitraryLable
    things = replicateM trials $ roundedUnitBoxPoints 2 d n
    trials = 10000
    delta = 0.01
    d = 2
    k = 4
    n = k

uniqueSlopesExperiment :: MonadRandom m =>
  Int -> Int -> Int -> Rational -> m ([Point Rational], Int)
uniqueSlopesExperiment n d trials delta
  = record numPrefs <$> bestPoints
  where bestPoints = most cmp trials generator
        generator = mapM phi [0, 1/n' .. 1 - 1/n' :: Rational]
        n' = fromIntegral n
        phi r = (\x -> [r,x]) <$> getRandomRat 5 (1-r, 1-r + 1/n')
        numPrefs = length . genLinearPrefs d delta . arbitraryLable
        cmp = compare `on` numPrefs

getRandomRat :: MonadRandom m => Int -> (Rational, Rational) -> m Rational
getRandomRat prec (low,high) = do
  i :: Integer <- getRandomR (0, 10^prec)
  let frac = toRational i / toRational (10^prec :: Integer)
  return $ low + frac * (high - low)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Specific 3D sets
-- -- -- -- -- -- -- -- -- -- -- -- -- -- --

oneNotWorst :: [[Double]]
oneNotWorst = [ [1,0.1,0.1], [0.1,1,0.1], [0.1,0.1,1], [0.5, 0.4, 0.6] ]

twoDominateTwo :: [[Double]]
twoDominateTwo -- = [ [1,0.1,1], [0.2,1,1], [0.2,0.1,1.5], [1,1,0.1] ]
  = [ [1,0,1], [0,1,1], [0,0,1.5], [1,1,0] ]

