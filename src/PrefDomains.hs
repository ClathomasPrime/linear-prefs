{-# LANGUAGE TupleSections #-}
module PrefDomains where

import Data.List

import Util
import LinearPref

import Debug.Trace


noOneFirst :: [[Int]]
noOneFirst = permutations [1..4]
  \\ fmap (1:) (permutations [2..4])

--------------------------------------------------------------------------------

fullSinglePeaked :: Eq a => [a] -> [[a]]
fullSinglePeaked as = nub $ do
  (left', right) <- listSplits as
  let left = reverse left'
  interleaves left right

--------------------------------------------------------------------------------

flipFlop :: [[Int]]
flipFlop =
  [ [1,2,  3,4]
  , [1,2,  4,3]
  , [2,1,  3,4]
  , [2,1,  4,3]
  ]

kCycle :: Int -> [[Int]]
kCycle k = rotations [1..k]

sandwich :: [[Int]]
sandwich = [ [1,2,3], [1,3,2], [3,2,1], [2,3,1] ]

mixedCompromise :: [[Int]]
mixedCompromise = [ [1,2,3,4], [2,1,4,3], [4,3,2,1] ]

goodCompromise :: [[Int]]
goodCompromise = [ [1,2,3], [2,1,3], [2,3,1], [3,2,1] ]

badCompromise :: [[Int]]
badCompromise = [ [1,2,3], [1,3,2], [3,1,2], [3,2,1] ]

superMedRestr :: [[Int]]
superMedRestr = [ [1,2,3,4], [2,1,4,3], [3,4,1,2], [4,3,2,1] ]

-- closure adds two elements. It's generalized S.C.
swingAround :: [[Int]]
swingAround = [ [1,2,3,4], [2,1,4,3], [3,2,1,4], [2,3,4,1] ]

-- the VR maximizer of swingAround: 1,3 << 2; 4 << 1,3; 4 << 2
-- WAIT it's not maximal... can add (only) [3,2,4,1] - makes it normal too
swingAroundSemiMaximal :: [[Int]]
swingAroundSemiMaximal = [[1,2,3,4],[2,1,3,4],[3,2,1,4],[2,3,1,4],[3,2,4,1],[2,3,4,1],[1,2,4,3],[2,1,4,3]]

swingAroundMaximal :: [[Int]]
swingAroundMaximal = [[1,2,3,4],[2,1,3,4],[3,2,1,4],[2,3,1,4],[3,2,4,1],[2,3,4,1],[1,2,4,3],[2,1,4,3],[3,4,2,1]]

-- NOTE: not value restricted and not condorcet!
flipInFlipOut :: [[Int]]
flipInFlipOut = [ [1,4,3,2], [2,4,3,1], [3,1,2,4], [3,2,1,4] ]

-- note: not closed (obviously). Closure just adds [1,2,4,3]
noRep :: [[Int]]
noRep = [ [1,2,3,4], [4,1,2,3], [2,1,4,3] ]

alphaConfig :: [[Int]]
alphaConfig = [ [4,1,2,3], [4,3,2,1] ]

betaOrbiter :: [[Int]]
betaOrbiter = [ [1,2,3,4], [2,4,1,3], [4,3,2,1], [3,1,4,2] ]

superGrpSep :: [[Int]]
superGrpSep =
  [ [1,2, 3,4]
  , [2,1, 4,3]
  , [3,4, 1,2]
  , [4,3, 2,1] ]

-- note: not closed
superGrpSep' :: [[Int]]
superGrpSep' =
  [ [1,2, 3,4]
  , [2,1, 4,3]
  , [3,4, 2,1]
  , [4,3, 1,2] ]

-- note: closure has seven elements
allThreeVR :: [[Int]]
allThreeVR =
  [ [1,2,3,4]
  , [2,4,3,1]
  , [2,1,4,3]
  , [3,2,4,1]
  , [3,4,2,1] ]


-- symmetrizing the basic generalized s.c. domain works here.
-- It doesn't work with just 4 outcomes.
symmetricExperiment :: [[Int]]
symmetricExperiment =
  [ [1,2,3,4,5,6]
  , [2,1,3,4,5,6]
  , [1,2,4,3,5,6]
  , [1,2,3,4,6,5]

  , [6,5,4,3,2,1]
  , [6,5,4,3,1,2]
  , [6,5,3,4,2,1]
  , [5,6,4,3,2,1]
  ]

-- It doesn't work with 5 outcomes either, at least in this basic way.
symmetricExperiment' :: [[Int]]
symmetricExperiment' =
  [ [1,2,3,4,5]
  , [2,1,3,4,5]
  , [1,2,3,5,4]
  , [1,3,2,4,5]

  , [5,4,3,2,1]
  , [5,4,3,1,2]
  , [4,5,3,2,1]
  , [5,4,2,3,1]
  ]



--------------------------------------------------------------------------------

-- Everything where 4 is first or second.
greatFourPoints :: [(Int,[Double])]
greatFourPoints = arbitraryLable
  [ [0.01, 0.01, 1]
  , [0.01, 1, 0.01]
  , [1, 0.01, 0.01]
  , [0.9, 0.9, 0.9]
  ]

greatFour :: [[Int]]
greatFour = filter ((4 `elem`) . take 2) $ permutations [1..4]

fourBeatsTwoPairs :: [[Int]]
fourBeatsTwoPairs = filter pred $ permutations [1..4]
  where pred pref =
          let p = takeWhile (/= 4) pref
           in not $ [1,2] `subset` p || [1,3] `subset` p

--------------------------------------------------------------------------------

-- [(1,[0.7498829556711089,3.648157068860303e-2,0.5618959726740518]),
-- (2,[0.32603968673739714,0.6740706017841367,0.6585671422681874]),
-- (3,[0.15130323704783166,0.7047433565451875,0.32674698754243214]),
-- (4,[0.6550978574590742,0.4893939665878215,0.734891271890886])]
crissCrosserPoints :: [(Int, [Double])]
crissCrosserPoints = arbitraryLable
  [ [1,   0.1, 0]
  , [0.5, 0.8, 1]
  , [0,   1.1, 0]
  , [0.8, 0.5, 1]
  ]
  -- [ [0.7,   0, 0.5]
  -- , [0.3, 0.6, 0.6]
  -- , [0.1, 0.65, 0.3]
  -- , [0.6, 0.55, 0.7]
  -- ]

crissCrosser' :: [[Int]]
crissCrosser' =
  [[4,2,1,3],[4,2,3,1],[2,4,3,1],[2,4,1,3],
    [2,3,4,1],[3,2,4,1],[4,1,2,3],[1,4,2,3]]

-- [[4,2,1,3],[4,2,3,1],[2,4,3,1],[2,3,4,1],[3,2,4,1],[4,1,2,3],[1,4,2,3]]
crissCrosser :: [[Int]]
crissCrosser =
  [[4,2,1,3],[4,2,3,1],[2,4,3,1],[2,3,4,1],[3,2,4,1],[4,1,2,3],[1,4,2,3]]

--------------------------------------------------------------------------------

doubleDown :: [(Int, [Double])]
doubleDown = [(1,[0.6818359476631632,0.3645786467558342,0.18733250761607811]),(2,[0.7363949964561536,0.22817942008667613,0.5799114436273074]),(3,[0.22464051370951488,0.42932412982380863,0.531062222291016]),(4,[0.2510393652446522,0.9515528812866416,0.3592532536246583])]

-- non-normalizeable (no reversed prefs possible); non-clique CD
exFourMaxCondDom :: [[Int]]
exFourMaxCondDom = [ [1,2,3,4], [2,3,1,4], [2,4,1,3], [2,1,4,3] ]

--------------------------------------------------------------------------------

uniqueSuccessor :: [[Int]]
uniqueSuccessor = [ [1,2,3,4], [1,3,2,4], [1,3,4,2], [4,1,3,2], [4,3,1,2], [4,3,2,1] ]
