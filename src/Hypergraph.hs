module Hypergraph where

import Data.List
import Data.Maybe
import Control.Monad

import GeneralD
import Util
import LinearPref

-- MODULE NOTE:: it might make waaay more sense to add a check for
-- wheter the pref domain even IS best/worst restricted.

-- (U, V) == (U << V) means you never have``all U above all V''
type Hypergraph a = [([a], [a])]

hypergraphOf :: Ord a => [[a]] -> [([a], [a])]
hypergraphOf prefs =
  let outcomes = sort . head $ prefs
      subpartitions = allSubpartitions outcomes

      legit (u,v) = all (notAllAboveIn u v) prefs
        -- make sure you don't have all of u before any element of v
   in filter legit subpartitions

notAllAboveIn :: Eq a => [a] -> [a] -> [a] -> Bool
notAllAboveIn u v pref =
  not $ u `subset` takeWhile (`notElem` v) pref

allSubpartitions :: [a] -> [([a], [a])]
allSubpartitions outcomes =
  filter nonNull $ fmap createPartition leftRightOrExcludeList
  where nonNull (u, v) = not (null u) && not (null v)
        accum e (_, Nothing) = e
        accum (u,v) (a, Just True) = (a:u, v)
        accum (u,v) (a, Just False) = (u, a:v)
        createPartition = (foldl accum ([],[]) . zip outcomes)
        leftRightOrExcludeList
          = nProd (length outcomes) [Just True, Just False, Nothing]

dominatePointPair :: ([a], [a]) -> Bool
dominatePointPair ([_], [_]) = True
dominatePointPair _ = False

minimalHypergraphOf :: Ord a => [[a]] -> Hypergraph a
minimalHypergraphOf prefs = filter (not . strictlyContained) graph
  where graph = hypergraphOf prefs
        strictlyContained (u, v) = any (p u v) graph
        p u v (u',v') = u' `subset` u && v' `subset` v
          && (u /= u' || v /= v')


--------------------------------------------------------------------------------

-- allMinimalHypergraphs :: Ord a => [a] -> [ Hypergraph a ]
-- allMinimalHypergraphs outcomes = nubBy eqUpToRelabeling $ do
--   let subparts = allSubpartitions outcomes
--   e:es <-

eqUpToRelabeling :: Ord a => [a] -> Hypergraph a -> Hypergraph a -> Bool
eqUpToRelabeling outcomes gr1 gr2 = any eqUnder permFuncs
  where eqUnder perm =
          (fmap . both) sort gr1 `eqAsSet` (fmap . both) (sort . fmap perm) gr2
        permFuncs = fmap funcOf (permutations outcomes)
        funcOf perm i = perm !! fromJust (i `elemIndex` outcomes)

valueRestricted :: Eq a => [a] -> Hypergraph a -> Bool
valueRestricted outcomes graph = all present tripples
  where tripples = [[a,b,c] | a:as <- tails outcomes, b:bs <- tails as, c <- bs]
        present l = any ((`subset` l) . (\(u,v) -> u ++ v)) graph


--------------------------------------------------------------------------------

-- Note: it suffices to plug a minimal hypergraph in here because
-- IF you satisfy u' << v' then you also
-- satisfy u << v for subsets u of u', v of v'

maxPrefSet :: Ord a => [a] -> Hypergraph a -> [[a]]
maxPrefSet outcomes graph =
  let satisfiesGraph pref = all (satisfiesEdge pref) graph
      satisfiesEdge pref (u,v) = notAllAboveIn u v pref
   in filter satisfiesGraph $ permutations (sort outcomes)

--------------------------------------------------------------------------------

hasDom :: Hypergraph a -> Bool
hasDom = any singletons
  where singletons ([_],[_]) = True
        singletons _ = False

experHyp :: IO [([(Int, Point Double)], Hypergraph Int)]
experHyp = do
  let n = 4
      outcomes = [1..n]
  hypergraphs <- replicateM 500 $ do
    points <- arbitraryLable <$> unitBoxPoints 3 n
    let prefSet = genLinearPrefs 3 0.005 $ points
        hypGraph = minimalHypergraphOf prefSet
    return (points, hypGraph)
  -- fmap catMaybes . forM hypergraphs $ \(points, graph) -> do
  --   if doubleDown graph
  --     then return $ Just points
  --     else return Nothing
  return $ nubBy (\u v -> eqUpToRelabeling outcomes (snd u) (snd v))
    . filter (not . hasDom . snd)
    . filter (not . valueRestricted outcomes . snd)
    $ hypergraphs

doubleDown :: Hypergraph a -> Bool
doubleDown gr =
  length gr >= 2 &&
  all (\(u,v) -> length u >= 2 && length v >= 2) gr
  -- -- prefSet <- randomFullPrefs 3 4 0.005

  -- if hasDom hypGraph
  --   then return ()
  --   else do
  --     print points
  --     print prefSet
  --     print hypGraph
  --     print ""

crissCrosserVRSystem :: Hypergraph Int
crissCrosserVRSystem = [([2,1],[4]),([3,1],[2]),([3,1],[4]),([4,3],[2])]

-- other cases...
-- [([2,1],[4]),([3,1],[4]),([2],[4,3])]
-- [([4,1],[2]),([4,3],[1]),([4,3],[2])]
--
-- [([4,1],[3,2]),([3,2],[4,1])]
--    ^ I think this is not real - just a loss of precision error.
--
-- [([3,2],[1]),([2],[4,1])]
-- [([3,2],[1]),([3],[4,1]),([3],[4,2])]
-- [([1],[4,2]),([3],[4,1]),([3],[4,2])]
-- [([2,1],[4]),([3,1],[4]),([3,2],[4])]
-- [([2],[4,1]),([3],[4,1])]
-- [([3,1],[2]),([1],[4,2]),([3,1],[4])]


-- [([2,1],[4]),([3,1],[4]),([2],[4,3])]
mixedCase :: [(Int, [Double])]
mixedCase =
 [(1,[0.8715701192754678,0.9684287158518972,5.522413688268646e-2]),
  (2,[0.6746957669018604,0.71666898566596,0.42663731462432664]),
  (3,[8.53787873802867e-2,0.530039848959367,0.9119598355047404]),
  (4,[0.97724435514835,0.9251726276870371,0.22461023138304892])
  ]

-- [([2,1],[4]),([3,1],[4]),([2],[4,3])]
mixedCaseNice :: [(Int, [Double])]
mixedCaseNice =
 [(1,[1,1,0]),
  (2,[0.6746957669018604,0.71666898566596,0.42663731462432664]),
  (3,[8.53787873802867e-2,0.530039848959367,0.9119598355047404]),
  (4,[0.97724435514835,0.9251726276870371,0.22461023138304892])
  ]

-- [([3,1],[2]),([1],[4,2]),([3,1],[4])]
mixedCase' :: [(Int, [Double])]
mixedCase' =
  [(1,[0.5735138414819833,0.6351329257968569,8.563476327044395e-2])
  ,(2,[0.9098078943166862,0.15754454932102924,0.8490297276463722])
  ,(3,[6.898901975163041e-2,5.391746370788764e-3,0.9357553713060002])
  ,(4,[0.3681468737776471,0.9625360308951275,0.8378442877244092])]
