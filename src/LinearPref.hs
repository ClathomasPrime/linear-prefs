module LinearPref where

import Data.List

type LinearPref l = [l]

compareByPref :: Eq l => [l] -> l -> l -> Ordering
compareByPref ranking x y
  | x == y = EQ
  | [Just i, Just j] <- (`elemIndex` ranking) <$> [x, y]
    = j `compare` i -- ^ reverse order, so earlier in list == better
  | otherwise = error "compareByPref where two args weren't in list"

consistentPrefs :: Eq l => [l] -> [[l]] -> [(l,l)]
consistentPrefs schools prefs =
  [ (x, y) | x <- schools, y <- schools,
    all (rankHigher x y) prefs ]
  where rankHigher x y p = compareByPref p x y == GT

arbitraryLable :: [x] -> [(Int, x)]
arbitraryLable = zip [1..]

subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize 0 _ = [[]]
subsetsOfSize _ [] = []
subsetsOfSize k (a:as) =
  ((a:) <$> subsetsOfSize (k-1) as) ++ subsetsOfSize k as

rotations :: [a] -> [[a]]
rotations as = undefined

hasKCycle :: Eq l => [l] -> Int -> [[l]] -> Bool
hasKCycle ls k prefs = undefined -- any cycleInPrefs subsets
  where cycleInPrefs c = all somewhereIn (map rotations c)
        somewhereIn _ = False
        subsets = subsetsOfSize k ls
