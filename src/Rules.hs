module Rules where

-- This module is abandoned for now. Maybe be useful when its time to generalize
-- to higher dimensions

import Util
import GeneralD
import LinearPref
import Control.Monad

data Implication l = Implication
  { hypothesis :: [(l,l)]
  , conclusion :: (l,l)
  } deriving(Eq, Ord)

instance Show l => Show (Implication l) where
  show (Implication hy concl) = show hy ++ " => " ++ show concl

conclusionsThatHold :: Eq l => [(l,l)] -> [[l]] -> [(l,l)]
conclusionsThatHold hypoths prefs
  = intersectTotalOrders . filter satisfyHypoths $ prefs
  where satisfyHypoths pref = all (`satisfies` pref) hypoths

nontrivialMinimalRules :: Eq l => [[l]] -> [Implication l]
nontrivialMinimalRules prefs = minimize $ nontrivialRules prefs
  where minimize = undefined

nontrivialRules :: Eq l => [l] -> Int -> [[l]] -> [Implication l]
nontrivialRules labels ruleLength prefs = do
  hypothesis <- setsOfTuples ruleLength labels
  -- guard . not . contradictory labels $ hypothesis -- ^ shouldn't reeeally be needed
  conclusion <- conclusionsThatHold hypothesis prefs
  guard . not $ conclusion `elem` hypothesis
  return $ Implication hypothesis conclusion


