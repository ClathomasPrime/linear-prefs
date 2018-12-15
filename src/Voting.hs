
module Voting where


import Control.Monad.Random
import Data.List
import Data.Function

import Util
import LinearPref
import GeneralD

type Space = [Point Double]

medianAngle :: [PrefWeight Double] -> PrefWeight Double
medianAngle = medianIndex . sortBy (compare `on` normedX)
  where normedX [x,y] = x / sqrt (sq x + sq y)
        normedX _ = undefined
        sq x = x^(2 :: Int)

-- weird encoding:
bimedian :: [[Double]] -> [Double]
bimedian = undefined

searchNonMonotone :: (MonadRandom m)
  => Int -> Int -> Int
  -> m (Maybe ([(Int,Point Double)], [PrefWeight Double]))
searchNonMonotone nTrials nWeights nCandidates =
  tryUntil nTrials (not . uncurry testMonotone) $ do
    candidates <- arbitraryLable <$> unitBoxPoints 2 nCandidates
    prefs <- unitCirclePrefs (nWeights + 2)
    return (candidates, prefs)



-- | First two in list will be swappy guys
searchNonMonotoneWeights :: (Ord l, MonadRandom m)
  => Int -> Int -> [(l, Point Double)] -> m (Maybe [PrefWeight Double])
searchNonMonotoneWeights nTrials nWeights candidates =
  tryUntil nTrials (not . testMonotone candidates)
    $ unitCirclePrefs (nWeights + 2)

testMonotone :: Ord l => [(l, Point Double)] -> [PrefWeight Double] -> Bool
testMonotone candidates allWeights =
  let origWeight:newWeight:sameWeights = allWeights
      origResult = head $ linearPref (medianAngle $ origWeight:sameWeights) candidates
      origPref = linearPref origWeight candidates
      newResult = head $ linearPref (medianAngle $ newWeight:sameWeights) candidates
      newPref = linearPref newWeight candidates
   in (origResult == newResult)
      || (greaterByPref origPref origResult newResult
          && greaterByPref newPref newResult origResult)

goodMiddle :: [(Char, Point Double)]
goodMiddle = [('a', [0.1,0.9]), ('b', [0.6,0.6]), ('c', [0.9,0.1])]

badMiddle :: [(Char, Point Double)]
badMiddle = [('a', [0.1,0.9]), ('b', [0.4,0.4]), ('c', [0.9,0.1])]

