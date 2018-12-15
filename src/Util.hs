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

medianIndex :: [a] -> a
medianIndex xs = xs !! (length xs `div` 2)
-- ^ Higher-index biased.


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
