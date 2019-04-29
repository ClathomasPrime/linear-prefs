module Hypergraph where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

import GeneralD
import Util
import LinearPref
import Voting

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
        strictlyContained (u, v) = any (pred u v) graph
        pred u v (u',v') = u' `subset` u && v' `subset` v
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
  where singletons ([a],[b]) = True
        singletons _ = False

experHyp :: IO [[(Int, [Double])]]
experHyp = do
  let n = 4
      outcomes = [1..n]
  hypergraphs <- replicateM 500 $ do
    points <- arbitraryLable <$> unitBoxPoints 3 n
    let prefSet = genLinearPrefs 3 0.005 $ points
        hypGraph = minimalHypergraphOf prefSet
    return (points, hypGraph)
  fmap catMaybes . forM hypergraphs $ \(points, graph) -> do
    if doubleDown graph
      then return $ Just points
      else return Nothing
    -- nubBy (eqUpToRelabeling outcomes)
    --   . filter (not . hasDom)
    --   . filter (not . valueRestricted outcomes)
    --   $ hypergraphs

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
