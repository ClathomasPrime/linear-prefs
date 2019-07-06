module Util where

import Control.Monad.Random
import Data.List

allDistinct :: Eq a => [a] -> Bool
allDistinct as = all (uncurry (/=)) pairs
  where pairs = [ (a,b) | a:bs <- tails as, b <- bs]

nubSort :: Ord a => [a] -> [a]
nubSort = map head . group . sort

fact :: Integral i => i -> i
fact n = product [1..n]

choose :: Integral i => i -> i -> i
choose n k = product [n, n-1.. n-k'+1] `div` fact k'
  where k' = min k (n - k)

record :: (a -> b) -> a -> (a, b)
record f a = (a, f a)

prod :: [a] -> [b] -> [(a,b)]
prod as bs = [(a,b) | a <- as, b <- bs]

nProd :: Int -> [a] -> [[a]]
nProd 0 _ = [[]]
nProd n as = [a:bs | a <- as, bs <- nProd (n-1) as]

setNProd :: Int -> [a] -> [[a]]
setNProd 0 _ = [[]]
setNProd n as = [x:bs | (x:xs) <- tails as, bs <- setNProd (n-1) xs]

setsOfTuples :: Eq a => Int -> [a] -> [[(a,a)]]
setsOfTuples n xs = setNProd n [(a,a') | a <- xs, a' <- xs, a /= a']

medianIndex :: [a] -> a
medianIndex xs = xs !! (length xs `div` 2)
-- ^ Higher-index biased.


listSplits :: [a] -> [([a],[a])]
listSplits [] = [([],[])]
listSplits (a:as) = ([],a:as) : map phi (listSplits as)
  where phi (u,v) = (a:u,v)

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,a') = (f a, f a')

--------------------------------------------------------------------------------

subset :: Eq a => [a] -> [a] -> Bool
subset as bs = all (`elem` bs) as

eqAsSet :: Eq a => [a] -> [a] -> Bool
eqAsSet as bs = null (as \\ bs) && null (bs \\ as)

eqUpToOrder :: Ord a => [a] -> [a] -> Bool
eqUpToOrder as bs = sort as == sort bs

symDiff :: Eq a => [a] -> [a] -> [a]
symDiff as bs = (as \\ bs) ++ (bs \\ as)

-- pass in a == type guy and get all its equiv classes
fullGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
fullGroupBy _ [] = []
fullGroupBy eq (a:as) =
  let (here, there) = partition (eq a) (a:as)
   in here : fullGroupBy eq there

-- pass in a <= type guy and get all its equiv classes
sortGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
sortGroupBy pred1 as = groupBy pred1 . sortBy pred' $ as
  where pred' a b
          | pred1 a b = GT
          | otherwise = LT

compareDivides :: Integral a => a -> a -> Ordering
compareDivides a b
  | a == b = EQ
  | a `rem` b == 0 = GT
  | otherwise = LT


-- Sorts such that, whenever pred a b, a comes before b
-- ASSUMING that pred a b, pred b c implies pred a c
sortPartial :: (a -> a -> Bool) -> [a] -> [a]
sortPartial p as = foldl ins [] as
  where ins [] a = [a]
        ins (x:xs) a
          | p a x = a:x:xs
          | otherwise = x : ins xs a

firstHalf :: [a] -> [a]
firstHalf as = take (length as `div` 2) as


adjacentPairs :: [a] -> [(a,a)]
adjacentPairs as = zip as $ tail' as
  where tail' [] = []
        tail' as' = tail as'

allPairs :: [a] -> [(a,a)]
allPairs outcomes = [(a,b) | a:as <- tails outcomes, b <- as]

allTriples :: [a] -> [(a,a,a)]
allTriples outcomes = [(a,b,c) | a:as <- tails outcomes, b:bs <- tails as, c <- bs]

--------------------------------------------------------------------------------

-- Lol @ this being here.
-- a_0 = 1, a_1 = 2; a_n = 3*a_{n-1}^2 - 2*a_{n-2}^4
stableMarriageLb :: Int -> Integer
stableMarriageLb 0 = 1
stableMarriageLb 1 = 2
stableMarriageLb n
  = 3 * (stableMarriageLb (n-1))^2 - 2*(stableMarriageLb (n-2))^4

--------------------------------------------------------------------------------

most :: Monad m => (a -> a -> Ordering) -> Int -> m a -> m a
most ord i gen = foldl1 keep <$> replicateM i gen
  where keep a b
          | b `ord` a == GT = b
          | otherwise = a

tryUntil :: Monad m => Int -> (a -> Bool) -> m a -> m (Maybe a)
tryUntil 0 _ _ = return Nothing
tryUntil nTrials test generator = do
  a <- generator
  if test a
     then return $ Just a
     else tryUntil (nTrials - 1) test generator

tryUntilSomething :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
tryUntilSomething 0 _ = return Nothing
tryUntilSomething nTrials generator = do
  a <- generator
  case a of
    Nothing -> tryUntilSomething (nTrials - 1) generator
    Just a' -> return $ Just a'

myNumFunc :: Int -> Int -> Int
myNumFunc n w = ((n-w) `c` 3) + ((n-w) `c` 1) * (w `c` 2)
  - ((n-w) `c` 2) * (w `c` 1) - (w `c` 3)
  where c m k
          | k > m || k < 0 = 1
          | otherwise = m `choose` k

swapDifference :: Eq l => [l] -> [l] -> Maybe (l,l)
swapDifference as bs =
  case filter (uncurry (/=)) $ zip as bs of
    [(x,y),_] -> Just (x,y)
    _ -> Nothing


sortPair :: Ord a => (a,a) -> (a,a)
sortPair (u,v)
  | u <= v = (u,v)
  | otherwise = (v,u)

myarbrec :: Int -> Int
myarbrec 2 = 2
myarbrec 3 = 4
myarbrec n = myarbrec (n-1) + n * myarbrec (n-2)
