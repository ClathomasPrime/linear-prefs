module ValRestr where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)

import GeneralD
import Util
import LinearPref
import Voting
import Condorcet


-- newtype VRSystem a = VRSystem
--   { vRSystem :: Map (a,a,a) -> (VRCase, a)
--   } deriving(Show, Eq, Ord)

type VRSystem a = [(VRCase,a,a,a)]

data VRCase
  = VRBestRestr
  -- ^ 1st never ahead of others
  | VRWorstRestr
  -- ^ 1st never behind of others
  | VRMediumRestr
  -- ^ 1st never between others
  deriving(Show, Eq, Ord)

-- data VRCase
--   = VRDominates a a
--   -- ^ 1st always before 2nd
--   | VRBestRestr a a a
--   -- ^ 1st never ahead of others
--   | VRWorstRestr a a a
--   -- ^ 1st never behind of others
--   | VRMediumRestr a a a
--   -- ^ 1st never between others

allVRSystems :: [a] -> [ VRSystem a ]
allVRSystems outcomes =
  let tripples = [(a,b,c) | a:as <- tails outcomes, b:bs <- tails as, c <- bs]
      cases = nProd (length tripples) [VRBestRestr, VRWorstRestr, VRMediumRestr]
      orders = nProd (length tripples) [1,2,3]
      zipZap (a,b,c) (x, 1) = (x,a,b,c)
      zipZap (a,b,c) (x, 2) = (x,b,a,c)
      zipZap (a,b,c) (x, 3) = (x,c,a,b)
      -- ^ I'll put a,b in lex order
   in fmap (zipWith zipZap tripples) (fmap (uncurry zip) $ prod cases orders)

satisfiesTripple :: Eq a => [a] -> (VRCase,a,a,a) -> Bool
satisfiesTripple pref (vrcase,a,b,c) =
  not $ case vrcase of
    VRBestRestr -> a `ahead` b && a `ahead` c
    VRWorstRestr -> b `ahead` a && c `ahead` a
    VRMediumRestr -> (b `ahead` a && a `ahead` c)
      || (c `ahead` a && a `ahead` b)
  where ahead u v = u `elem` takeWhile (/= v) pref

maxPrefSet :: Ord a => [a] -> VRSystem a -> [[a]]
maxPrefSet outcomes sys =
  let satisfiesSys pref = all (satisfiesTripple pref) sys
   in filter satisfiesSys $ permutations (sort outcomes)

eqUpToRelabeling :: Ord a => [a] -> VRSystem a -> VRSystem a -> Bool
eqUpToRelabeling outcomes sys1 sys2 = any eqUnder permFuncs
  where eqUnder perm =
          fmap justSort sys1 `eqAsSet` fmap (applyPerm perm) sys2
        justSort (vrcase, a,b,c) = (vrcase, a, sort $ [b,c])
        applyPerm perm (vrcase,a,b,c) = (vrcase, perm a, sort $ fmap perm [b,c])
        permFuncs = fmap funcOf (permutations outcomes)
        funcOf perm i = perm !! fromJust (i `elemIndex` outcomes)

maximalVRSystems :: Ord a => [a] -> [ VRSystem a ]
maximalVRSystems outcomes = filter yieldsMaximal . allVRSystems $ outcomes
  where yieldsMaximal sys =
          let dom = maxPrefSet outcomes sys
           in if dom == []
                then False
                else isMaximal dom

























haveIsoGraphs :: Ord a => [a] -> VRSystem a -> VRSystem a -> Bool
haveIsoGraphs outcomes sys1 sys2 =
  let set1 = arbitraryLable . maxPrefSet outcomes $ sys1
      gr1 = condorcetGraphEdges $ set1
      set2 = arbitraryLable . maxPrefSet outcomes $ sys2
      gr2 = condorcetGraphEdges $ set2
   in if length set1 == length set2
         then isoGraph [1..length set1] gr1 gr2
         else False

-- fullGroupBy (haveIsoGraphs [1..4]) casesOnFour
-- equivClassesOfGraph :: [ [VRSystem Int] ]
-- equivClassesOfGraph =
--   [ [[(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRWorstRestr,3,2,4)],[(VRBestRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]]
--   , [[(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,3,2,4)],[(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]]
--   , [[(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,4,2,3)],[(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]]
--   , [[(VRBestRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)],[(VRWorstRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]]
--   ]

equivClassesOfGraph :: [ [VRSystem Int] ]
equivClassesOfGraph =
  [
    -- 8
    [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,1,3,4),(VRBestRestr,2,3,4)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]
    ]

    -- 8
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,3,1,4),(VRBestRestr,3,2,4)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
    ]

    -- 7. These two are the single crossing domains (line graphs)
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRWorstRestr,3,2,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
    ]

    -- 8
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRWorstRestr,4,2,3)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRWorstRestr,2,3,4)]
    ]

    -- 8
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]
    ]

    -- 6
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,4,1,3),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    ]

    -- 7
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,4,1,2),(VRMediumRestr,4,1,3),(VRWorstRestr,4,2,3)]
    , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    ]

    -- 8
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    ]

    -- 9 - the unique maximum
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
    ]

    -- 8
  , [ [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]
    ]

    -- 8
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,3,2,4)]
    ]

    -- 8 verts. Graph is a line of squares:
    -- _______
    -- | | | |
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRWorstRestr,3,1,2),(VRWorstRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRWorstRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]
    ]

    -- 6
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]
    ]

    -- 6
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRWorstRestr,3,1,2),(VRWorstRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]
    ]

    -- 7
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,3,2,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRMediumRestr,3,1,4),(VRMediumRestr,3,2,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,3,2,4)]
    ]

    -- 6
  , [ [(VRBestRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,2,3,4)]
    , [(VRWorstRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,2,3,4)]
    ]

    -- 8. 3 dimensional hypercubes
  , [ [(VRMediumRestr,1,2,3),(VRMediumRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,3,1,4),(VRMediumRestr,4,2,3)]
    ]

    -- 4. 2 dimensional hypercubes
  , [ [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,3,2,4)]
    ]
  ]

-- according to my code, these are the noniso cases that give maximal VR systems
-- nubBy (eqUpToRelabeling [1,2,3,4]) $ maximalVRSystems [1,2,3,4]
casesOnFour :: [VRSystem Int]
casesOnFour =
  [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,1,3,4),(VRBestRestr,2,3,4)]
  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,3,1,4),(VRBestRestr,3,2,4)]

  , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRWorstRestr,3,2,4)]
  , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRWorstRestr,4,2,3)]

  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,4,1,3),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,4,2,3)]

  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]

  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
  , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]

  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]
  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,3,2,4)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]

  , [(VRBestRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
  , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRWorstRestr,2,3,4)]

  , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
  , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,3,2,4)]
  , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,4,2,3)]
  , [(VRBestRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]

  , [(VRBestRestr,1,2,3),(VRWorstRestr,4,1,2),(VRMediumRestr,4,1,3),(VRWorstRestr,4,2,3)]

  , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRMediumRestr,3,1,4),(VRMediumRestr,3,2,4)]
  , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]

  , [(VRBestRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]

  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]
  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]

  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]

  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]
  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,3,1,4),(VRMediumRestr,3,2,4)]
  , [(VRWorstRestr,3,1,2),(VRWorstRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,3,1,2),(VRWorstRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,3,2,4)]

  , [(VRWorstRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]

  -- these two have eight preferences, and corresp resp to 1*(2*(3*4)) and (1*2)*(3*4)
  , [(VRMediumRestr,1,2,3),(VRMediumRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,3,1,4),(VRMediumRestr,4,2,3)]

  -- this is beta orbiter:
  , [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,3,2,4)]
  ]
