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

rankHigher :: Eq l => l -> l -> [l] -> Bool
rankHigher x y p = compareByPref p x y == GT

consistentPrefs :: Eq l => [l] -> [[l]] -> [(l,l)]
consistentPrefs schools prefs =
  [ (x, y) | x <- schools, y <- schools, all (rankHigher x y) prefs ]

arbitraryLable :: [x] -> [(Int, x)]
arbitraryLable = zip [1..]

alphaLable :: [x] -> [(Char, x)]
alphaLable = zip ['a'..]

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
toOrderedPairs ls =
  [ (x,y) | x:ys <- tails ls, y <- ys ]
  -- do -- lol I overthought this huh
  -- (front,back) <- listSplits ls
  -- case reverse front of
  --   (a:_) -> fmap (a,) back
  --   _ -> []

-- fromOrderedPairs assumes all (n choose 2) total order pairs are given
fromOrderedPairs :: Eq l => [l] -> [(l,l)] -> [l]
fromOrderedPairs outcomes orders = reverse $ sortBy cmp outcomes
  where cmp a b
          | (a,b) `elem` orders = GT
          | (b,a) `elem` orders = LT
          | otherwise = error "ordered pair unexpected stuff occured idk"

-- checks and enforces that ALL (n choose 2) orders are given
fromOrderedPairsFull :: Eq l => [l] -> [(l,l)] -> Maybe [l]
fromOrderedPairsFull [] _ = Just []
fromOrderedPairsFull outcomes prefs = do
  let beatsAll x = all (\y -> any (== (x,y)) prefs) (outcomes \\ [x])
        && not (any (\y -> any (== (y,x)) prefs) (outcomes \\ [x]))
  winner <- find beatsAll outcomes
  -- traceShowM winner
  let rest = outcomes \\ [winner]
  restOrder <- fromOrderedPairsFull rest prefs
  return $ winner : restOrder

intersectTotalOrders :: Eq l => [[l]] -> [(l,l)]
intersectTotalOrders ls =
  foldl1 intersect . fmap toOrderedPairs $ ls

satisfies :: Eq l => [l] -> (l,l) -> Bool
satisfies ls (a,b) =
  case (elemIndex a ls, elemIndex b ls) of
    (Just ax, Just bx) -> ax < bx
    _ -> False -- ^ Maybe not a good default but ?

-- contradictory :: Eq l => [l] -> [(l,l)] -> Bool
-- contradictory labels tuples = undefined

-- not well-named
dominated :: Eq l => (l,l) -> [[l]] -> Bool
dominated (a,b) prefs = all dom prefs
  where dom pref = greaterByPref pref a b

--------------------------------------------------------------------------------

