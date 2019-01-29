{-# LANGUAGE TupleSections #-}
module LinearPref where

import Data.List

import Util

type LinearPref l = [l]

compareByPref :: Eq l => [l] -> l -> l -> Ordering
compareByPref ranking x y
  | x == y = EQ
  | [Just i, Just j] <- (`elemIndex` ranking) <$> [x, y]
    = j `compare` i -- ^ reverse order, so earlier in list == better
  | otherwise = error "compareByPref where two args weren't in list"

greaterByPref :: Eq l => [l] -> l -> l -> Bool
greaterByPref ranking x y = compareByPref ranking x y == GT

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

interleaves :: [a] -> [a] -> [[a]]
interleaves as [] = [as]
interleaves [] bs = [bs]
interleaves (a:as) (b:bs)
  = fmap (a:) (interleaves as (b:bs))
  ++ fmap (b:) (interleaves (a:as) bs)

rotations :: [a] -> [[a]]
rotations as = map (\(x,y) -> y ++ x) (init $ listSplits as)

sublist :: Eq l => [l] -> [l] -> Bool
sublist [] _ = True
sublist _ [] = False
sublist (l:ls) (x:xs)
  | l == x = sublist ls xs
  | otherwise = sublist (l:ls) xs

hasPattern :: Eq l => [[l]] -> [[l]] -> Bool
hasPattern patterns prefs = all present patterns
  where present p = any (p `sublist`) prefs

hasKCycle :: Eq l => [l] -> Int -> [[l]] -> Bool
hasKCycle ls k prefs = any cycleInPrefs subsets
  where cycleInPrefs c = all somewhereIn (rotations c)
        somewhereIn t = any (t `sublist`) prefs
        subsets = subsetsOfSize k ls

--------------------------------------------------------------------------------

-- Convention: A pair (a,b) denotes a>b.

toOrderedPairs :: [l] -> [(l,l)]
toOrderedPairs ls = do
  (front,back) <- listSplits ls
  case reverse front of
    (a:_) -> fmap (a,) back
    _ -> []

-- fromOrderedPairs assumes all (n choose 2) total order pairs are given
fromOrderedPairs :: Eq l => [l] -> [(l,l)] -> [l]
fromOrderedPairs outcomes orders = reverse $ sortBy cmp outcomes
  where cmp a b
          | (a,b) `elem` orders = GT
          | (b,a) `elem` orders = LT
          | otherwise = error "ordered pair unexpected stuff occured idk"

intersectTotalOrders :: Eq l => [[l]] -> [(l,l)]
intersectTotalOrders ls =
  foldl1 intersect . fmap toOrderedPairs $ ls

satisfies :: Eq l => (l,l) -> [l] -> Bool
satisfies (a,b) ls =
  case (elemIndex a ls, elemIndex b ls) of
    (Just ax, Just bx) -> ax < bx
    _ -> False -- ^ Maybe not a good default but ?

contradictory :: Eq l => [l] -> [(l,l)] -> Bool
contradictory labels tuples = undefined

-- not well-named
dominated :: Eq l => (l,l) -> [[l]] -> Bool
dominated (a,b) prefs = all dom prefs
  where dom pref = greaterByPref pref a b

--------------------------------------------------------------------------------

noOneFirst :: [[Int]]
noOneFirst = permutations [1..4]
  \\ fmap (1:) (permutations [2..4])

--------------------------------------------------------------------------------

fullSinglePeaked :: Eq a => [a] -> [[a]]
fullSinglePeaked as = nub $ do
  (left', right) <- listSplits as
  let left = reverse left'
  interleaves left right

--------------------------------------------------------------------------------

flipFlop :: [[Int]]
flipFlop =
  [ [1,2,  3,4]
  , [1,2,  4,3]
  , [2,1,  3,4]
  , [2,1,  4,3]
  ]

kCycle :: Int -> [[Int]]
kCycle k = rotations [1..k]

sandwich :: [[Int]]
sandwich = [ [1,2,3], [1,3,2], [3,2,1], [2,3,1] ]

goodCompromise :: [[Int]]
goodCompromise = [ [1,2,3], [2,1,3], [2,3,1], [3,2,1] ]

badCompromise :: [[Int]]
badCompromise = [ [1,2,3], [1,3,2], [3,1,2], [3,2,1] ]

