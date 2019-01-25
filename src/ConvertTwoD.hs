{-# LANGUAGE TupleSections #-}
module ConvertTwoD where

import Data.Function
import Data.List
import Data.Tuple

import Util
import LinearPref

--------------------------------------------------------------------------------

data TwoDSpec l = TwoDSpec
  { outcomes :: [l]
  , fixedPairs :: [(l,l)]
  , pivotList :: [[(l,l)]]
  } deriving(Show)

specsToPrefs :: Eq l => TwoDSpec l -> [[l]]
specsToPrefs (TwoDSpec outcomes fixed pivots)
  = fmap (fromOrderedPairs outcomes . buildPref) $ listSplits pivots
  where buildPref (flip,leave)
          = fixed ++ (concat $ leave ++ fmap (fmap swap) flip)
          -- fromOrderedPairs outcomes

checkImplies :: Eq l => [[l]] -> (l,l) -> (l,l) -> Bool
checkImplies prefs (x,y) (u,v)
  = all (\p -> greaterByPref p u v) (filter (\p -> greaterByPref p x y) prefs)

separateDominatedPairs :: Eq l => [[l]] -> ([(l,l)], [(l,l)])
separateDominatedPairs prefs
  = (filter fixed pairs, filter free pairs)
  where outcomes = head prefs
        fixed (a,b) = all (\pref -> greaterByPref pref a b) prefs
        pairs = [(l,l') | l <- outcomes, l' <- outcomes, l /= l']
        free p = not (fixed p) && not (fixed $ swap p)

convertTwoD :: Eq l => [[l]] -> TwoDSpec l
convertTwoD prefs =
  let outcomes = head prefs
      (fixedPairs, freePairs) = separateDominatedPairs prefs
      sorted = sortPartial (checkImplies prefs) $ freePairs
      connected = filter (checkImplies prefs (head sorted)) sorted
        -- note: safe by laziness of filter
      equiv p q = checkImplies prefs p q && checkImplies prefs q p
   in TwoDSpec outcomes fixedPairs (groupBy equiv connected)

--------------------------------------------------------------------------------


prefs01, prefs02, prefs03, prefs04 :: [[Int]]

prefs01 = [ [2,4,1,3], [2,3,4,1], [2,4,3,1], [3,2,4,1] ]

-- gr = pairImplicationGraph [1..4] prefs02 -- componentOf (1,3) $

prefs02 = [ [2,1,4,3], [2,4,1,3], [4,2,3,1] ]

prefs03 = [ [1,2,4,3], [2,4,1,3], [1,3,4,2], [1,3,2,4] ]

prefs04 = [ [1,2,3], [3,2,1] ]

prefs05 = [ [1,3,2,4], [1,2,4,3], [1,4,2,3], [1,2,3,4] ]

--------------------------------------------------------------------------------

checkAlg :: Ord l => [[l]] -> Maybe [[l]]
checkAlg prefs
  | ((==) `on` sort) prefs (specsToPrefs $ convertTwoD prefs) = Nothing
  | otherwise = Just prefs
-- e.g. fmap checkAlg <$> replicateM 100 (randPrefs d 10 1000)
