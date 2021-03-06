module ValRestr where

import Data.List
import Data.Maybe
import Control.Monad.Random

import Util
import LinearPref
import Condorcet


-- newtype VRSystem a = VRSystem
--   { vRSystem :: Map (a,a,a) -> (VRCase, a)
--   } deriving(Show, Eq, Ord)
-- data VRCase
--   = VRDominates a a
--   -- ^ 1st always before 2nd
--   | VRBestRestr a a a
--   -- ^ 1st never ahead of others
--   | VRWorstRestr a a a
--   -- ^ 1st never behind of others
--   | VRMediumRestr a a a
--   -- ^ 1st never between others

type VRSystem a = [(VRCase,a,a,a)]

data VRCase
  = VRBestRestr
  -- ^ 1st never ahead of others
  | VRWorstRestr
  -- ^ 1st never behind of others
  | VRMediumRestr
  -- ^ 1st never between others
  deriving(Show, Eq, Ord)

allVRSystems :: [a] -> [ VRSystem a ]
allVRSystems outcomes =
  let tripples = [(a,b,c) | a:as <- tails outcomes, b:bs <- tails as, c <- bs]
      cases = nProd (length tripples) [VRBestRestr, VRWorstRestr, VRMediumRestr]
      orders = nProd (length tripples) [1,2,3 :: Int]
      zipZap (a,b,c) (x, 1) = (x,a,b,c)
      zipZap (a,b,c) (x, 2) = (x,b,a,c)
      zipZap (a,b,c) (x, 3) = (x,c,a,b)
      zipZap _ _ = error "impossible thing in allVRSystems"
      -- ^ I'll put a,b in lex order
   in fmap (zipWith zipZap tripples) (fmap (uncurry zip) $ prod cases orders)


-- assumes you contain 1 2 ... n
mostVRSystems :: [a] -> [ VRSystem a ]
mostVRSystems outcomes =
  let triples = allTriples outcomes
      cases = nProd (length triples) [VRBestRestr, VRWorstRestr, VRMediumRestr]
      orders = nProd (length triples) [True, False]
      -- ^ I'll put a,b in lex order
   in fmap (zipWith mostVRZipZap triples) (fmap (uncurry zip) $ prod cases orders)

mostVRZipZap :: (a,a,a) -> (VRCase, Bool) -> (VRCase, a,a,a)
mostVRZipZap (a,b,c) (x@VRBestRestr, True) = (x,b,a,c)
mostVRZipZap (a,b,c) (x@VRBestRestr, False) = (x,c,a,b)
mostVRZipZap (a,b,c) (x@VRWorstRestr, True) = (x,a,b,c)
mostVRZipZap (a,b,c) (x@VRWorstRestr, False) = (x,b,a,c)
mostVRZipZap (a,b,c) (x@VRMediumRestr, True) = (x,a,b,c)
mostVRZipZap (a,b,c) (x@VRMediumRestr, False) = (x,c,a,b)

-- assumes you contain 1 2 ... n
randomVRSystem :: MonadRandom m => [a] -> m (VRSystem a)
randomVRSystem outcomes = mapM pickCase (allTriples outcomes)
  where pickCase (i,j,k) = do
          thing <- fromList [ ((x,y),1)
            | x <- [VRBestRestr, VRWorstRestr, VRMediumRestr]
            , y <- [True, False] ]
          return $ mostVRZipZap (i,j,k) thing

-- assumes you contain 1 2 ... n and its reverse.
randomNormPeakPitVRSystem :: MonadRandom m => [a] -> m (VRSystem a)
randomNormPeakPitVRSystem outcomes = mapM pickCase (allTriples outcomes)
  where zipZap (a,b,c) x = (x,b,a,c)
        pickCase (i,j,k) = do
          thing <- fromList [ (x,1) | x <- [VRBestRestr, VRWorstRestr] ]
          return $ zipZap (i,j,k) thing

-- assumes you contain 1 2 ... n and its reverse.
randomNormalVRSystem :: MonadRandom m => [a] -> m (VRSystem a)
randomNormalVRSystem outcomes = mapM pickCase (allTriples outcomes)
  where pickCase (i,j,k) = do
          thing <- fromList [ (x,1) |
            x <- [(VRBestRestr, Nothing), (VRWorstRestr, Nothing),
             (VRMediumRestr, Just True), (VRMediumRestr, Just False)] ]
          return $ zipZap (i,j,k) thing

        zipZap (a,b,c) (vrcase, Nothing) = (vrcase, b, a, c)
        zipZap (a,b,c) (vrcase, Just True) = (vrcase, a, b, c)
        zipZap (a,b,c) (vrcase, Just False) = (vrcase, c, a, b)

-- note: if you contain both
-- 1 2 3
-- 3 2 1
-- then you can extend this to:
--   - 2 not best
--   - 2 not worst
--   - 1 not between 2,3:
--        1 3 2
--        2 3 1
--   - 3 not between 1,2:
--        2 1 3
--        3 1 2

normalVRSystems :: [a] -> [ VRSystem a ]
normalVRSystems outcomes =
  let triples = [(a,b,c) | a:as <- tails outcomes, b:bs <- tails as, c <- bs]
      cases = nProd (length triples)
        [(VRBestRestr, Nothing), (VRWorstRestr, Nothing),
         (VRMediumRestr, Just True), (VRMediumRestr, Just False)]
      zipZap (a,b,c) (vrcase, Nothing) = (vrcase, b, a, c)
      zipZap (a,b,c) (vrcase, Just True) = (vrcase, a, b, c)
      zipZap (a,b,c) (vrcase, Just False) = (vrcase, c, a, b)
   in fmap (zipWith zipZap triples) cases

normalPeakPitVRSystems :: [a] -> [ VRSystem a ]
normalPeakPitVRSystems outcomes =
  let triples = [(a,b,c) | a:as <- tails outcomes, b:bs <- tails as, c <- bs]
      cases = nProd (length triples) [VRBestRestr, VRWorstRestr]
      zipZap (a,b,c) x = (x,b,a,c)
      -- ^ I'll put a,b in lex order
   in fmap (zipWith zipZap triples) cases

-- NOTE: we assume 1,2,...,n is in the domain, so this isn't all of them
peakPitVRSystems :: [a] -> [ VRSystem a ]
peakPitVRSystems outcomes =
  let triples = allTriples outcomes
      cases = nProd (length triples)
        [(x,y) | x <- [VRBestRestr, VRWorstRestr], y <- [True, False] ]
      zipZap (a,b,c) (vr@VRBestRestr, True) = (vr, b, a, c)
      zipZap (a,b,c) (vr@VRBestRestr, False) = (vr, c, a, b)
      zipZap (a,b,c) (vr@VRWorstRestr, True) = (vr, a, b, c)
      zipZap (a,b,c) (vr@VRWorstRestr, False) = (vr, b, a, c)
      zipZap _ _ = error "bad call ref that's a bad call"
   in fmap (zipWith zipZap triples) cases

-- NOTE: we assume 1,2,...,n is in the domain, so this isn't all of them
peakVrSystems :: [a] -> [ VRSystem a ]
peakVrSystems outcomes =
  let triples = allTriples outcomes
      cases = nProd (length triples) [True, False]
      zipZap (a,b,c) True = (VRWorstRestr, a, b, c)
      zipZap (a,b,c) False = (VRWorstRestr, b, a, c)
   in fmap (zipWith zipZap triples) cases

satisfiesTripple :: Eq a => [a] -> (VRCase,a,a,a) -> Bool
satisfiesTripple pref (vrcase,a,b,c) =
  not $ case vrcase of
    VRBestRestr -> a `ahead` b && a `ahead` c
    VRWorstRestr -> b `ahead` a && c `ahead` a
    VRMediumRestr -> (b `ahead` a && a `ahead` c)
      || (c `ahead` a && a `ahead` b)
  where ahead u v = u `elem` takeWhile (/= v) pref

-- also tells you which tripples were not full restriction
vrSystemWithPartials :: Ord a => [[a]] -> (VRSystem a, [(a,a,a)])
vrSystemWithPartials prefs =
  let outcomes = head prefs
      triples = [(a,b,c) | a:as <- tails outcomes, b:bs <- tails as, c <- bs]
      accum (castTriples, uncastTripples) (x,y,z) =
        case identifyVRCase x y z prefs of
          Left res -> (res:castTriples, uncastTripples)
          Right _ -> (castTriples, (x,y,z):uncastTripples)
   in foldl accum ([], []) triples

projectDown :: Eq a => [a] -> [[a]] -> [[a]]
projectDown goodOutcomes = nub . fmap (filter (`elem` goodOutcomes))

removeDown :: Eq a => [a] -> [[a]] -> [[a]]
removeDown badOutcomes = nub . fmap (filter (`notElem` badOutcomes))

identifyVRCase :: Ord a => a -> a -> a -> [[a]]
  -> Either (VRCase,a ,a,a) () -- ^ unit for now, may be info in the future
identifyVRCase x y z prefs =
  let proj = nub $ fmap (filter (`elem` [x,y,z])) prefs
      nonFirstSlots = [x,y,z] \\ fmap (!! 0) proj
      nonMiddleSlots= [x,y,z] \\ fmap (!! 1) proj
      nonLastSlots  = [x,y,z] \\ fmap (!! 2) proj
      maxNot u = maximum $ [x,y,z] \\ [u]
      minNot u = minimum $ [x,y,z] \\ [u]
   in case (nonFirstSlots, nonMiddleSlots, nonLastSlots) of
        ([u], [], []) -> Left (VRBestRestr, u, maxNot u, minNot u)
        ([], [u], []) -> Left (VRMediumRestr, u, maxNot u, minNot u)
        ([], [], [u]) -> Left (VRWorstRestr, u, maxNot u, minNot u)
        _ -> Right ()

-- very lossy and throws info away
vrSystemOf :: Ord a => [[a]] -> VRSystem a
vrSystemOf = fst . vrSystemWithPartials

maxPrefSet :: Ord a => [a] -> VRSystem a -> [[a]]
maxPrefSet outcomes sys =
  let satisfiesSys pref = all (satisfiesTripple pref) sys
   in filter satisfiesSys $ permutations outcomes

eqUpToRelabeling :: Ord a => [a] -> VRSystem a -> VRSystem a -> Bool
eqUpToRelabeling outcomes sys1 sys2 = any eqUnder permFuncs
  where eqUnder perm =
          fmap justSort sys1 `eqAsSet` fmap (applyPerm perm) sys2
        justSort (vrcase, a,b,c) = (vrcase, a, sort $ [b,c])
        applyPerm perm (vrcase,a,b,c) = (vrcase, perm a, sort $ fmap perm [b,c])
        permFuncs = fmap funcOf (permutations outcomes)
        funcOf perm i = perm !! fromJust (i `elemIndex` outcomes)

maximalVRSystems :: Ord a => [a] -> [ VRSystem a ]
maximalVRSystems outcomes = filter (yieldsMaximal outcomes) . allVRSystems $ outcomes

yieldsMaximal :: Ord a => [a] -> VRSystem a -> Bool
yieldsMaximal outcomes sys =
  let dom = maxPrefSet outcomes sys
   in if dom == []
        then False
        else isMaximal dom

graphOfValRest :: [Int] -> VRSystem Int -> [(Char,Char)]
graphOfValRest outcomes sys
  = condorcetGraphEdges . fmap starAlpha . alphaLable . maxPrefSet outcomes $ sys
  where starAlpha (c,pref)
          | sort pref == pref = ('*', pref)
          | otherwise = (c,pref)

graphOfValRest' :: [Int] -> VRSystem Int -> [([Int],[Int])]
graphOfValRest' outcomes sys
  = condorcetGraphEdges' . maxPrefSet outcomes $ sys

haveIsoGraphsSorta :: Ord a => [a] -> VRSystem a -> VRSystem a -> Bool
haveIsoGraphsSorta outcomes sys1 sys2 =
  let set1 = arbitraryLable . maxPrefSet outcomes $ sys1
      gr1 = condorcetGraphEdges $ set1
      set2 = arbitraryLable . maxPrefSet outcomes $ sys2
      gr2 = condorcetGraphEdges $ set2
   in sameDegreeMultiset [1..length set1] gr1 gr2
      -- if length set1 == length set2
      --    then isoGraph [1..length set1] gr1 gr2
      --    else False

checkFullProjToTripple :: Eq a => [a] -> [[a]] -> Bool
checkFullProjToTripple tripple prefs =
  4 == (length . nub . fmap (filter (`elem` tripple))) prefs

checkFullProjToAllTripples :: Eq a => [[a]] -> Bool
checkFullProjToAllTripples prefs =
  let outcomes = head prefs
      triples = [[a,b,c] | a:as <- tails outcomes, b:bs <- tails as, c <- bs]
   in all (flip checkFullProjToTripple prefs) triples

splitUpperBoundExper :: IO (Maybe (Int, VRSystem Int))
splitUpperBoundExper = tryUntilSomething 1000 $ do
  let n = 7::Int
      outcomes = [1..n]
  split <- getRandomR (1,n-1)
  sys <- randomNormPeakPitVRSystem outcomes
  let total = length . maxPrefSet outcomes $ sys

      isLeft (_,u,v,w) = all (<=split+1) [u,v,w]
      leftSys = filter isLeft sys
      left = length . maxPrefSet [1..split+1] $ leftSys

      isRight (_,u,v,w) = all (>=split) [u,v,w]
      rightSys = filter isRight sys
      right = length . maxPrefSet [split..n] $ rightSys
  -- if total < left
  --   then return . Just $ (split, sys)
  --   else return Nothing
  if total <= left * right
    then do
      -- print $ show total ++ "<=" ++ show left ++ "*" ++ show right
      return Nothing
    else do
      print $ "DANGER"
      return . Just $ (split, sys)

