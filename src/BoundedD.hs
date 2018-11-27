module BoundedD where

import Control.Monad.Random
import Data.List

import Util

-- TODO: rename this
type R2 = (Double, Double)

pointsInUnitBox :: MonadRandom m => m R2
pointsInUnitBox = (,) <$> x <*> x
  where x = getRandomR (0.0,1.0)

pointsToTenthInUnitBox :: MonadRandom m => m R2
pointsToTenthInUnitBox = (,) <$> x <*> x
  where x = do y <- getRandomR (1,9 :: Int)
               return $ fromIntegral y / 10

pointsInUnitCircle :: MonadRandom m => m R2
pointsInUnitCircle = rcis <$> radius <*> angle
  where rcis r t = (r * cos t, r * sin t)
        radius = getRandomR (0, 1)
        angle = getRandomR (0, pi / 2)

arbitraryLable :: [a] -> [(Int, a)]
arbitraryLable rs = zip [0..] rs

type R3 = (Double, Double, Double)

pointsInUnitCube :: MonadRandom m => m R3
pointsInUnitCube = (,,) <$> x <*> x <*> x
  where x = getRandomR (0.0,1.0)

pointsToTenthInUnitCube :: MonadRandom m => m R3
pointsToTenthInUnitCube = (,,) <$> x <*> x <*> x
  where x = do y <- getRandomR (1,9 :: Int)
               return $ fromIntegral y / 10

pointsToHundredthInUnitCube :: MonadRandom m => m R3
pointsToHundredthInUnitCube = (,,) <$> x <*> x <*> x
  where x = do y <- getRandomR (1,99 :: Int)
               return $ fromIntegral y / 100

--------------------------------------------------------------------------------

type PreferenceWeight x = x -> Double

anglePreference :: Double -> PreferenceWeight R2
anglePreference t = \(x,y) -> cos t * x + sin t * y

rankingOf :: Ord a => PreferenceWeight b -> [(a, b)] -> [a]
rankingOf p xs =
  reverse . fmap fst . sortBy cmp $ xs
  where cmp (a,x) (b,y) = (p x `compare` p y) <> a `compare` b
-- (compare `on` p . snd)

sweep :: Ord a => [Double] -> [(a,R2)] -> [[a]]
sweep angles points = nub $ fmap rank angles
  where rank theta = rankingOf (anglePreference theta) points

tightSweep :: Ord a => [(a,R2)] -> [[a]]
tightSweep = sweep [0,pi/1024..pi/2]

tightSweep3 :: Ord a => [(a,R3)] -> [[a]]
tightSweep3 points = nub $ fmap rank angles
  where rank anglePair = rankingOf (prefOf anglePair) points
        prefOf (theta,phi) (x,y,z)
          = x * (cos theta * cos phi)
          + y * (sin theta * cos phi)
          + z * (sin phi)
        angles = (,) <$> [0,pi/256..pi/2] <*> [0,pi/256..pi/2]

--------------------------------------------------------------------------------

compareByPref :: Eq a => [a] -> a -> a -> Ordering
compareByPref ranking a b
  | a == b = EQ
  | [Just i, Just j] <- (`elemIndex` ranking) <$> [a, b]
    = j `compare` i -- ^ reverse order, so earlier in list == better
  | otherwise = error "compareByPref where two args weren't in list"


consistentPrefs :: Eq a => [[a]] -> [(a,a)]
consistentPrefs prefs =
  [ (a, b) | a <- schools, b <- schools,
    all (rankHigher a b) prefs ]
  where rankHigher a b p = compareByPref p a b == GT
        schools = nub . concat $ prefs


--------------------------------------------------------------------------------

--------------------

complexPrefPoints :: [(Int, R2)]
complexPrefPoints =
  [ (0, (0.2, 0.6))
  , (1, (0.9, 0.2))
  , (2, (0.6, 0.1))
  , (3, (0.8, 0.4))
  , (4, (0.7, 0.9))
  ]

-- complexPrefSet == tightSweep complexPrefPoints
complexPrefSet :: [[Int]]
complexPrefSet =
  [ [1,3,4,2,0]
  , [1,4,3,2,0]
  , [4,1,3,2,0]
  , [4,3,1,2,0]
  , [4,3,1,0,2]
  , [4,3,0,1,2]
  , [4,0,3,1,2]
  ]

--------------------

manyPrefPoints :: [(Int, [(Int, R2)])]
manyPrefPoints =
  [ (10, [(0,(0.6,0.6)),(1,(0.7,0.3)),(2,(0.5,0.6)),(3,(0.8,0.4)),(4,(0.2,0.7))])
  , (9, [(0,(0.7,0.2)),(1,(0.9,0.4)),(2,(0.5,0.8)),(3,(0.2,0.5)),(4,(0.6,0.4))])
  , (8, [(0,(0.2,0.8)),(1,(0.5,0.3)),(2,(0.3,0.2)),(3,(0.6,0.6)),(4,(0.2,0.7))])
  , (6, [(0,(0.5,0.7)),(1,(0.8,0.2)),(2,(0.7,0.3)),(3,(0.9,0.8)),(4,(0.6,0.2))])
  , (3, [(0,(0.5,0.3)),(1,(0.1,0.2)),(2,(0.7,0.8)),(3,(0.1,0.6)),(4,(0.8,0.7))])
  ]

--------------------

-- yields exactly three preferences:
--   (0,1,...,n), the reverse, and all tied.
--   (probably best to tiebreak in a uniform way)
diagonalPrefPoints :: Int -> [(Int, R2)]
diagonalPrefPoints n =
  [ (floor i, (i/n',1 - i/n')) | i <- [0..n']]
    where n' = fromIntegral n :: Double

--------------------

-- note: has n+1 points
unitCirclePrefPoints :: Int -> [(Int, R2)]
unitCirclePrefPoints n =
  [ (i, r i) | i <- [0..n]]
  where r i = let theta = (pi / 2 / fromIntegral n) * fromIntegral i
               in (cos theta, sin theta)

-- note: has n points
interiorCirclePrefPoints :: Int -> Double -> [(Int, R2)]
interiorCirclePrefPoints n rho =
  [ (i, r i) | i <- [0..n-1]]
  where r i = let theta = rho + (2 * pi / fromIntegral n) * fromIntegral i
               in (0.5 + 0.3 * cos theta, 0.5 + 0.3 * sin theta)

-- Result of the buggy version of the above:
-- (( siiigh... when "/ fromIntegral n" was "* fromIntegral n", things
--   got really weird. Rounding error?))
-- nutsStuff =
--   [ (0,(0.398,0.65))
--   , (1,(0.405,0.6))
--   , (2,(0.413,0.54))
--   , (3,(0.422,0.5))
--   , (4,(0.43 ,0.44))
--   ]

--------------------------------------------------------------------------------

searchBigPref :: MonadRandom m => m (Maybe [(Int, R3)])
searchBigPref = do
  let kUnitPoints k = arbitraryLable <$> replicateM k pointsToTenthInUnitCube
      bigPrefSet (prefs,_) = length prefs >= 18
  manyPoints <- replicateM 100 (kUnitPoints 4)
  let xs = take 1 . filter bigPrefSet . fmap (record tightSweep3) $ manyPoints
  case xs of
    [] -> return Nothing
    ((_,y):_) -> return . Just $ y

{-
maximalWithThree = [(0,(0.1,0.9)),(1,(0.6,0.6)),(2,(0.9,0.1))]
maximalWithFour =
  [ (0,(0.1,0.6,0.8))
  , (1,(0.3,0.3,0.9))
  , (2,(0.9,0.7,0.2))
  , (3,(0.7,0.1,0.9))
  ]

maximalWithFourPrefs =
  [ [2,3,1,0]
  , [3,2,1,0]
  , [3,1,2,0]
  , [3,1,0,2]
  , [2,3,0,1]
  , [3,2,0,1]
  , [3,0,1,2]
  , [2,0,3,1]
  , [3,0,2,1]
  , [0,3,2,1]
  , [0,2,3,1]
  , [0,3,1,2]
  , [2,0,1,3]
  , [0,2,1,3]
  , [0,1,3,2]
  , [1,3,0,2]
  , [0,1,2,3]
  , [1,0,3,2]
  ]
  -}


-- A maximal set of prefs from randomly chosen points (maxWithFour)
-- [0,1,2,3]
-- [0,1,3,2]
-- [0,2,1,3]
-- [0,2,3,1]
-- [0,3,1,2]
-- [0,3,2,1]
--
-- [1,0,3,2]
-- [1,3,0,2]
--
-- [2,0,1,3]
-- [2,0,3,1]
-- [2,3,0,1]
-- [2,3,1,0]
--
-- [3,0,1,2]
-- [3,0,2,1]
-- [3,1,0,2]
-- [3,1,2,0]
-- [3,2,0,1]
-- [3,2,1,0]
--
-- Same thing as above, arranged by orbit.
-- [0,1,2,3]
-- [2,3,0,1]
-- [3,0,1,2]
--
-- [0,1,3,2]
-- [3,2,0,1]
-- [2,0,1,3]
--
-- [0,2,1,3]
-- [1,3,0,2]
-- [3,0,2,1]
--
-- [0,2,3,1]
-- [2,3,1,0]
-- [3,1,0,2]
--
-- [0,3,1,2]
-- [3,1,2,0]
-- [2,0,3,1]
--
-- [0,3,2,1]
-- [1,0,3,2]
-- [3,2,1,0]


-- None these have 3 as the favorite.
--   That, plus a counting argument shows that this must be the maximal
--   collection of permuations not containing a full rotation
-- [0,1,2,3]
-- [0,1,3,2]
-- [0,2,1,3]
-- [0,2,3,1]
-- [0,3,1,2]
-- [0,3,2,1]
--
-- [1,0,2,3]
-- [1,0,3,2]
-- [1,2,0,3]
-- [1,2,3,0]
-- [1,3,0,2]
-- [1,3,2,0]
--
-- [2,0,1,3]
-- [2,0,3,1]
-- [2,1,0,3]
-- [2,1,3,0]
-- [2,3,0,1]
-- [2,3,1,0]
--
--   Q: is there a maximal guy with 3 never the favorite which ISN'T
--     just a relabeling of the above?
--



--
-- This setting sometimes prefers 3. Unlike the randomly generated
--   maximal guy, this one has a uniform number of instances that
--   most prefer k, for k =/= 3. All of S{0,1,2} follow 3 in some prefs.
--     (in the randomly generated one, all of S{0,1,2} follows both 3 and 0)
-- [0,1,3,2]
-- [0,2,3,1]
-- [0,3,1,2]
-- [0,3,2,1]
--
-- [1,0,3,2]
-- [1,2,3,0]
-- [1,3,0,2]
-- [1,3,2,0]
--
-- [2,0,3,1]
-- [2,1,3,0]
-- [2,3,0,1]
-- [2,3,1,0]
--
-- [3,0,1,2]
-- [3,0,2,1]
-- [3,1,0,2]
-- [3,1,2,0]
-- [3,2,0,1]
-- [3,2,1,0]
