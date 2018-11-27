module Util where

import Control.Monad.Random
import Data.List
import Data.Function

fact :: Integral i => i -> i
fact n = product [1..n]

record :: (a -> b) -> a -> (b, a)
record f a = (f a, a)

prod :: [a] -> [b] -> [(a,b)]
prod as bs = [(a,b) | a <- as, b <- bs]

nProd :: Int -> [a] -> [[a]]
nProd 0 _ = [[]]
nProd n as = [a:bs | a <- as, bs <- nProd (n-1) as]

--------------------------------------------------------------------------------

most :: Monad m => (a -> a -> Ordering) -> Int -> m a -> m a
most ord i gen = foldl1 keep <$> replicateM i gen
  where keep a b
          | b `ord` a == GT = b
          | otherwise = a

