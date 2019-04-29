
module Voting where


import Control.Monad.Random
import Data.Maybe
import Data.List
import Data.Function

import Util
import LinearPref
import GeneralD

import Debug.Trace


majorityPrefer :: Eq l => l -> l -> [[l]] -> Bool
majorityPrefer x y prefs = nPrefXY > nVoters `div` 2
  where nVoters = length prefs
        nPrefXY = length . filter (rankHigher x y) $ prefs

majorityPairs :: Ord l => [[l]] -> [(l,l)]
majorityPairs prefs = sort $ do
  let outcomes = head prefs
  x:ys <- tails outcomes
  y <- ys
  if majorityPrefer x y prefs
    then return (x,y)
    else if majorityPrefer y x prefs
           then return (y,x)
           else []

-- WARNING: undefined behavior on non-condorcet inputs.
majorityRule :: Ord l => [[l]] -> [l]
majorityRule ls = fromOrderedPairs (head ls) (majorityPairs ls)

majorityRuleSafe :: Ord l => [[l]] -> Maybe [l]
majorityRuleSafe ls = fromOrderedPairsFull (head ls) (majorityPairs ls)


--------------------------------------------------------------------------------
-- DIMENSIONAL VOTING
--------------------------------------------------------------------------------

medianAngle :: [PrefWeight Double] -> PrefWeight Double
medianAngle = medianIndex . sortBy (compare `on` normedX)
  where normedX [x,y] = x / sqrt (sq x + sq y)
        normedX _ = undefined
        sq x = x^(2 :: Int)

-- weird encoding:
bimedian :: [[Double]] -> [Double]
bimedian = undefined

--------------------------------------------------------------------------------

searchNonMonotone :: (MonadRandom m)
  => Int -> Int -> Int
  -> m (Maybe ([(Int,Point Double)], [PrefWeight Double]))
searchNonMonotone nTrials nWeights nCandidates =
  tryUntil nTrials (not . uncurry testMonotone) $ do
    candidates <- arbitraryLable <$> unitBoxPoints 2 nCandidates
    prefs <- unitCirclePrefs (nWeights + 1)
    -- traceShowM $ [linearPref p candidates | p <- prefs]
    return (candidates, prefs)

-- | First two in list will be swappy guys
searchNonMonotoneWeights :: (Ord l, Show l, MonadRandom m)
  => Int -> Int -> [(l, Point Double)] -> m (Maybe [PrefWeight Double])
searchNonMonotoneWeights nTrials nWeights candidates =
  tryUntil nTrials (not . testMonotone candidates)
    $ unitCirclePrefs (nWeights + 1)

testMonotone :: (Ord l, Show l) => [(l, Point Double)] -> [PrefWeight Double] -> Bool
testMonotone candidates allWeights =
  let origWeight:newWeight:sameWeights = allWeights
      origResult = head $ linearPref (medianAngle $ origWeight:sameWeights) candidates
      origPref = linearPref origWeight candidates
      newResult = head $ linearPref (medianAngle $ newWeight:sameWeights) candidates
      newPref = linearPref newWeight candidates
   in (origResult == newResult)
      || (
        traceShow (fmap (flip linearPref candidates) (origWeight:sameWeights),
          origResult,newPref,newResult)
          $ greaterByPref origPref origResult newResult
          && greaterByPref newPref newResult origResult)

--------------------------------------------------------------------------------

searchNonCondorcet :: (MonadRandom m)
  => Int -> Int -> Int
  -> m (Maybe ([(Int,Point Double)], [PrefWeight Double]))
searchNonCondorcet nTrials nWeights nCandidates =
  tryUntil nTrials (not . uncurry testCondorcet) $ do
    candidates <- arbitraryLable <$> unitBoxPoints 2 nCandidates
    prefs <- unitCirclePrefs (nWeights)
    return (candidates, prefs)

testCondorcet :: (Show l, Ord l) => [(l, Point Double)] -> [PrefWeight Double] -> Bool
testCondorcet candidates weights =
  isJust . testCondorcetLinear (fmap fst candidates)
  $ fmap (flip linearPref candidates) weights

testCondorcetLinear :: (Show l, Ord l) => [l] -> [[l]] -> Maybe l
testCondorcetLinear candidates prefs
  = find (\c -> all (majorityInHeadToHead c) candidates) candidates
  where majorityInHeadToHead c d | c == d = True
        majorityInHeadToHead c d =
          (length . filter (\p -> greaterByPref p c d)) prefs > maj
        maj = length candidates `div` 2
