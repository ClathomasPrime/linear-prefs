
module Voting where


import Control.Monad.Random
import Data.Maybe
import Data.List
import Data.Function

import Util
import LinearPref
import GeneralD

import Debug.Trace

type Space = [Point Double]

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

goodMiddle :: [(Char, Point Double)]
goodMiddle = [('a', [0.1,0.9]), ('b', [0.6,0.6]), ('c', [0.9,0.1])]

badMiddle :: [(Char, Point Double)]
badMiddle = [('a', [0.1,0.9]), ('b', [0.4,0.4]), ('c', [0.9,0.1])]

