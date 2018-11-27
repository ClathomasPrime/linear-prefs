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

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (a:as) = ([],a:as) : map phi (splits as)
  where phi (u,v) = (a:u,v)

rotations :: [a] -> [[a]]
rotations as = map (\(x,y) -> y ++ x) (init $ splits as)

sublist :: Eq l => [l] -> [l] -> Bool
sublist [] _ = True
sublist _ [] = False
sublist (l:ls) (x:xs)
  | l == x = sublist ls xs
  | otherwise = sublist (l:ls) xs

hasKCycle :: Eq l => [l] -> Int -> [[l]] -> Bool
hasKCycle ls k prefs = any cycleInPrefs subsets
  where cycleInPrefs c = all somewhereIn (rotations c)
        somewhereIn t = any (t `sublist`) prefs
        subsets = subsetsOfSize k ls

