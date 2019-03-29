module Util where

import Control.Monad.Random
import Data.List
import Data.Function

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

--------------------------------------------------------------------------------

subset :: Eq a => [a] -> [a] -> Bool
subset as bs = all (`elem` bs) as

eqAsSet :: Eq a => [a] -> [a] -> Bool
eqAsSet as bs = null (as \\ bs) && null (bs \\ as)

sortGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
sortGroupBy pred as = groupBy pred . sortBy pred' $ as
  where pred' a b
          | pred a b = GT
          | otherwise = LT

compareDivides :: Integral a => a -> a -> Ordering
compareDivides a b
  | a == b = EQ
  | a `rem` b == 0 = GT
  | otherwise = LT


-- Sorts such that, whenever pred a b, a comes before b
-- ASSUMING that pred a b, pred b c implies pred a c
sortPartial :: (a -> a -> Bool) -> [a] -> [a]
sortPartial pred as = foldl insert [] as
  where insert [] a = [a]
        insert (x:xs) a
          | pred a x = a:x:xs
          | otherwise = x : insert xs a

firstHalf :: [a] -> [a]
firstHalf as = take (length as `div` 2) as


adjacentPairs :: [a] -> [(a,a)]
adjacentPairs as = zip as $ tail' as
  where tail' [] = []
        tail' as = tail as

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
    Just a -> return $ Just a

phi :: Int -> Int -> Int
phi n w = ((n-w) `c` 3) + ((n-w) `c` 1) * (w `c` 2)
  - ((n-w) `c` 2) * (w `c` 1) - (w `c` 3)
  where c n k
          | k > n || k < 0 = 1
          | otherwise = n `choose` k
