module LPData where

import SingleCrossing
import LinearPref



--------------------------------------------------------------------------------

drag1Drag2 :: SingleCrossingSpec Int
drag1Drag2 = SingleCrossingSpec [1..4] [(3,4)]
  [ [(1,2)], [(1,3)], [(1,4)], [(2,3)], [(2,4)] ]

drag1Drag2Prefs :: [[Int]]
drag1Drag2Prefs = specToPrefs drag1Drag2

foundDrag :: [(Int, [Double])]
foundDrag = arbitraryLable $
  [ [0.4, 0.7 ]
  , [0.7, 0.6 ]
  , [0.8, 0.3 ]
  , [0.75,0.1 ]
  ]
  -- [ [0.42454071900099377,0.6471117341499713]
  -- , [0.6888613783614072,0.6292102931464019]
  -- , [0.8099845863883132,0.34652947029878567]
  -- , [0.7600276384499445,0.13731477855356722]
  -- ]

--------------------------------------------------------------------------------

exOne :: SingleCrossingSpec Int
exOne = SingleCrossingSpec
  [1..4] [(1,3),(1,4),(2,3),(2,4)] [[(1,2)], [(3,4)]]

baseEx :: SingleCrossingSpec Int
baseEx = SingleCrossingSpec
  [1..4] [(1,2),(3,4)] [[(2,3)],[(2,4)],[(1,3)],[(1,4)]]

keyEx :: SingleCrossingSpec Int
keyEx = SingleCrossingSpec
  [1..4] []
    [ [(3,2), (4,2)], [(1,2)], [(4,3)], [(1,3), (1,4)] ]

keyExPoints :: [[Double]]
keyExPoints =
  [ [10.844660881198175, 1.0]
  , [1.0, 13.344814282762075]
  , [2.9498558243636466, 4.801937735804838]
  , [6.463374613663353, 2.0]
  ]

doubleCross :: SingleCrossingSpec Int
doubleCross = SingleCrossingSpec
  [1..4] [(2,3)] [ [(1,2),(3,4)], [(1,4)], [(2,4),(1,3)] ]

doubleDrag :: SingleCrossingSpec Int
doubleDrag = SingleCrossingSpec
  [1..4] [(2,3),(2,4)] [[(1,2),(1,3)], [(1,4),(3,4)]]

doubleDragContainedInPoints :: [[Double]]
doubleDragContainedInPoints =
  [ [10 , 1.0]
  , [7.6, 8.6]
  , [6.6, 5.8]
  , [1.0, 7.6]
  ]

doubleDragContainedInPoints' :: [[Double]]
doubleDragContainedInPoints' =
  [ [10 , 1]
  , [7, 8]
  , [6, 5]
  , [1, 7]
  ]

heresOne :: SingleCrossingSpec Int
heresOne = (SingleCrossingSpec {scOutcomes = [1,2,3,4,5], scFixedPairs = [(1,2),(1,4),(1,5),(3,4),(3,5)], scPivotList = [[(4,5)],[(2,3)],[(1,3)],[(2,5)],[(2,4)]]})

-- this set works for heresOne
heresOnePoints :: [[Double]]
heresOnePoints =
  [ [ 20.28691827016931, 14.956504889899058 ]
  , [ 19.28691827016931, 1.0 ]
  , [ 4.949855824363648, 28.466452061746686 ]
  , [ 3.9498558243636475, 5.52630128284193 ]
  , [ 1.0, 13.956504889899058 ]
  ]

hardOne6 :: SingleCrossingSpec Int
hardOne6 = SingleCrossingSpec
  { scOutcomes = [1,2,3,4,5,6]
  , scFixedPairs = [(2,3),(4,6),(5,6)]
  , scPivotList = [[(4,5)],[(1,2)],[(1,3)],[(1,5)],[(1,4)],[(3,5)],[(2,5)],[(1,6)],[(3,4)],[(3,6)],[(2,4)],[(2,6)]]
  }

-- fixed pref angles doesn't work
hardOne5 :: SingleCrossingSpec Int
hardOne5 = SingleCrossingSpec
  { scOutcomes = [1,2,3,4,5]
  , scFixedPairs = []
  , scPivotList = fmap (:[])
      [ (3,4),(2,4),(3,5),(1,4),(2,5),(1,5),(4,5),(2,3),(1,3),(1,2) ]
  }

hardOne5Points :: [[Double]]
hardOne5Points =
  [ [20,1]

  , [17,1.5]
  , [13.1,2.5]
  , [13,10]

  , [1,20]
  ]

-- fixed x and fixed crosses doesn't work.
hardOne4 :: SingleCrossingSpec Int
hardOne4 = SingleCrossingSpec
  { scOutcomes = [1,2,3,4]
  , scFixedPairs = [(1,2)]
  , scPivotList = fmap (:[]) [ (2,3), (1,3), (2,4), (1,4), (3,4) ]
  }

-- hard for fixed crosses but doable for fixed x
-- (``complete inversion'' with (2,3) to make it hard for fixed x)
hardOne4Try :: SingleCrossingSpec Int
hardOne4Try = SingleCrossingSpec
  { scOutcomes = [1,2,3,4]
  , scFixedPairs = [(1,2)]
  , scPivotList = fmap (:[]) [ (3,4), (2,4), (1,4), (2,3), (1,3){-, (2,3) -} ]
  }

hardOne4TryPoints :: [[Double]]
hardOne4TryPoints = [ [2,4], [1,3], [3,2], [11,1] ]



-- Actually works with fixed angles for pref
hard5Attempt1 :: SingleCrossingSpec Int
hard5Attempt1 = SingleCrossingSpec
  { scOutcomes = [1,2,3,4,5]
  , scFixedPairs = []
  , scPivotList = fmap (:[]) [(2,3), (1,3), (2,4), (1,4), (3,4), (2,5), (1,5), (3,5), (4,5), (1,2)]
  }

-- eg this guy
hard5Attempt1Points :: [[Double]]
hard5Attempt1Points =
  [ [ 1573,1] , [ 1423,30] , [ 1339,591] , [ 1038,906] , [ 1,1194] ]

hard5Attempt2 :: SingleCrossingSpec Int
hard5Attempt2 = SingleCrossingSpec
  { scOutcomes = [1,2,3,4,5]
  , scFixedPairs = []
  , scPivotList = fmap (:[])
      []
      -- [(1,2),
  }

hard6SparseFlips :: SingleCrossingSpec Int
hard6SparseFlips = SingleCrossingSpec
  { scOutcomes = [1,2,3,4,5,6]
  , scFixedPairs = [(1,3),(1,5),(2,3),(2,5),(4,5)]
  , scPivotList = fmap (:[])
      [ (5,6),(4,6),(3,6),(1,2),(1,6),(3,4),(1,4),(3,5),(2,6),(2,4) ]
  }

hard6Guess1 :: SingleCrossingSpec Int
hard6Guess1 = SingleCrossingSpec
  { scOutcomes = [1,2,3,4,5,6]
  , scFixedPairs = [(1,3),(1,5),(2,3),(2,5),(4,5)]
  , scPivotList = fmap (:[])
      [ (1,2),(3,4),(5,6), (1,4),(3,6), (2,4),(1,6),(3,5),
        (2,6),(1,5), (4,6),(2,5),(1,3), (4,5),(2,3) ]
  }
