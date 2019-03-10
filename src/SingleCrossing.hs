{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SingleCrossing where

import Control.Monad.Random as Rand
import Data.Function
import Data.List
import Data.Tuple

import Util
import LinearPref
import Debug.Trace

data SingleCrossingSpec l = SingleCrossingSpec
  { outcomes :: [l]
  , fixedPairs :: [(l,l)]
  , pivotList :: [[(l,l)]]
  -- ^ Q: best to make this just a list?
  } deriving(Show)

extremalPrefs :: Eq l => SingleCrossingSpec l -> ([l], [l])
extremalPrefs (SingleCrossingSpec outcomes fixeds pivots)
  = ( fromOrderedPairs outcomes (fixeds ++ concat pivots),
    fromOrderedPairs outcomes (fixeds ++ fmap swap (concat pivots)) )

separateDominatedPairs :: Eq l => [[l]] -> ([(l,l)], [(l,l)])
separateDominatedPairs prefs
  = (filter fixed pairs, filter free pairs)
  where outcomes = head prefs
        fixed (a,b) = all (\pref -> greaterByPref pref a b) prefs
        pairs = [(l,l') | l <- outcomes, l' <- outcomes, l /= l']
        free p = not (fixed p) && not (fixed $ swap p)

--------------------------------------------------------------------------------


specToPrefs :: Eq l => SingleCrossingSpec l -> [[l]]
specToPrefs (SingleCrossingSpec outcomes fixed pivots)
  = fmap (fromOrderedPairs outcomes . buildPref) $ listSplits pivots
  where buildPref (flip,leave)
          = fixed ++ (concat $ leave ++ fmap (fmap swap) flip)
          -- fromOrderedPairs outcomes

checkImplies :: Eq l => [[l]] -> (l,l) -> (l,l) -> Bool
checkImplies prefs (x,y) (u,v)
  = all (\p -> greaterByPref p u v) (filter (\p -> greaterByPref p x y) prefs)


convertTwoD :: Eq l => [[l]] -> SingleCrossingSpec l
convertTwoD prefs =
  let outcomes = head prefs
      (fixedPairs, freePairs) = separateDominatedPairs prefs
      sorted = sortPartial (checkImplies prefs) $ freePairs
      connected = filter (checkImplies prefs (head sorted)) sorted
        -- note: safe by laziness of filter
      equiv p q = checkImplies prefs p q && checkImplies prefs q p
   in SingleCrossingSpec outcomes fixedPairs (groupBy equiv connected)

--------------------------------------------------------------------------------

randSingCrossing :: MonadRandom m => Int -> m (SingleCrossingSpec Int)
randSingCrossing n = do
  (prefs, SingleCrossingSpec outcomes _ pivots)  <-
    combobulate (SingleCrossingSpec [1..n] [] []) [1..n]
  let (fixedPairs, _) = separateDominatedPairs prefs
  -- traceShowM prefs
  return $ SingleCrossingSpec outcomes fixedPairs (reverse pivots)

combobulate :: MonadRandom m
  => SingleCrossingSpec Int -> [Int] -> m ([[Int]], SingleCrossingSpec Int)
combobulate sc@(SingleCrossingSpec outcomes fixeds pivots) current = do
  let pairs = adjacentPairs current \\
        concatMap (\x -> [x,swap x]) (fixeds ++ concat pivots)
        -- include swap because reasons
  if null pairs
    then return ([current], sc)
    else do
      index <- getRandomR (0,length pairs - 1)
      let pair = pairs !! index
      flip <- Rand.fromList [(True, 0.8), (False, 0.2)]
      (restList, restSc) <- if flip
        then combobulate
               (SingleCrossingSpec outcomes fixeds $ [pair]:pivots)
               (flipPair pair current)
        else combobulate
               (SingleCrossingSpec outcomes (pair:fixeds) pivots)
               current
      return (current:restList, restSc)

flipPair :: Eq a => (a, a) -> [a] -> [a]
flipPair (a,b) as =
  let pairsList = adjacentPairs as
      (blockOne, blockTwo) = span (/= (a,b)) pairsList
   in if null blockTwo
        then as
        else fmap fst blockOne ++ [b,a] ++ fmap snd (tail blockTwo)

-- flipGenerate :: SingleCrossingSpec a ->

--------------------------------------------------------------------------------


prefs01, prefs02, prefs03, prefs04 :: [[Int]]

prefs01 = [ [2,4,1,3], [2,3,4,1], [2,4,3,1], [3,2,4,1] ]

-- gr = pairImplicationGraph [1..4] prefs02 -- componentOf (1,3) $

prefs02 = [ [2,1,4,3], [2,4,1,3], [4,2,3,1] ]

prefs03 = [ [1,2,4,3], [2,4,1,3], [1,3,4,2], [1,3,2,4] ]

prefs04 = [ [1,2,3], [3,2,1] ]

prefs05 = [ [1,3,2,4], [1,2,4,3], [1,4,2,3], [1,2,3,4] ]

--------------------------------------------------------------------------------

checkAlg :: Ord l => [[l]] -> Maybe [[l]]
checkAlg prefs
  | ((==) `on` sort) prefs (specToPrefs $ convertTwoD prefs) = Nothing
  | otherwise = Just prefs
-- e.g. fmap checkAlg <$> replicateM 100 (randPrefs d 10 1000)
