module ValRestrCases where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)

import ValRestr



-- fullGroupBy (haveIsoGraphs [1..4]) casesOnFour
equivClassesOfGraph :: [ [VRSystem Int] ]
equivClassesOfGraph =
  [
    -- non normal.
    -- 8. 8 edges. Graph: square with legs (of two edges) off adjacents
    [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,1,3,4),(VRBestRestr,2,3,4)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]
    ]

    -- normal
    -- 8, 8 edges. Graph: square with galaxy arms (of two edges) off diagonal.
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,3,1,4),(VRBestRestr,3,2,4)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
    -- ^ single peaked with base order 2, 1, 3, 4
    ]

    -- normal
    -- 7. 7 edges. The maximal single crossing domains (line graphs)
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRWorstRestr,3,2,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
    ]

    -- non normal
    -- 8. 8 edges. Square, length 1 legs off adjacents, plus length two leg.
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRWorstRestr,4,2,3)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRWorstRestr,2,3,4)]
    ]

    -- index 4 in list. non normal
    -- 8 verts. 9 edges.
    -- Two squares in a line with length two legs (of length 1):
    -- ______
    -- |_|_|_
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]
    ]

    -- normal
    -- 7. 7 edges. Square with length 3 tail
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    ]

    -- Index 6 in list. normal
    -- 8. 9 edges. Squares in a line with a leg of length 2 off an end:
    -- ________
    -- |_|_|
  , [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    ]

    -- normal
    -- 9 - the unique maximum. 10 edges
    -- Bridgey diamonds with a tail. See median graphs paper.
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
    ]

    -- non normal
    -- Index 8 in list.
    -- 8. 8 edges. Pinwheel: square with length 1 edge off each side.
  , [ [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]
    ]

    -- normal.
    -- 8. 9 edges. Looks like: (with length 1 legs)
    -- ______
    --  |_|_|__
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,3,2,4)]
    ]

    -- normal.
    -- 8 verts. Graph is a line of squares:
    -- _______
    -- |_|_|_|
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRWorstRestr,3,1,2),(VRWorstRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRWorstRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]
    ]

    -- normal. Indeed, symmetric
    -- these two have eight preferences, and corresp resp to 1*(2*(3*4)) and (1*2)*(3*4)
    -- 8. 3 dimensional hypercubes
  , [ [(VRMediumRestr,1,2,3),(VRMediumRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
    , [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,3,1,4),(VRMediumRestr,4,2,3)]
    ]

    -- normal. Indeed, symmetric
    -- 4. 2 dimensional hypercubes
    -- this is beta orbiter:
  , [ [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,3,2,4)]
    ]
  ]



-- okay not even this is true - some have two successors... but still
-- these popped out from some old bugs so they might be interesting.
casesWithUniqueSuccessor :: [ [VRSystem Int] ]
casesWithUniqueSuccessor =
  [
    -- 7
    [ [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,1,2,3),(VRWorstRestr,4,1,2),(VRMediumRestr,4,1,3),(VRWorstRestr,4,2,3)]
    ]
    -- 6
  , [ [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRBestRestr,4,1,3),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
    , [(VRBestRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
    , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
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
  , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,2,1,3),(VRBestRestr,2,1,4),(VRBestRestr,3,1,4),(VRMediumRestr,4,2,3)]

  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]

  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]

  , [(VRBestRestr,1,2,3),(VRBestRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRBestRestr,3,1,2),(VRBestRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]

  , [(VRBestRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]
  , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRWorstRestr,2,3,4)]

  , [(VRBestRestr,1,2,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]
  , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,3,2,4)]
  , [(VRBestRestr,2,1,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,4,2,3)]

  , [(VRBestRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]

  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRWorstRestr,2,3,4)]
  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,3,1,4),(VRWorstRestr,3,2,4)]

  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRWorstRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,2,1,3),(VRWorstRestr,2,1,4),(VRWorstRestr,3,1,4),(VRMediumRestr,4,2,3)]

  , [(VRWorstRestr,1,2,3),(VRWorstRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRWorstRestr,3,1,2),(VRWorstRestr,4,1,2),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]

  , [(VRWorstRestr,1,2,3),(VRMediumRestr,4,1,2),(VRMediumRestr,4,1,3),(VRMediumRestr,4,2,3)]

  , [(VRMediumRestr,1,2,3),(VRMediumRestr,1,2,4),(VRMediumRestr,1,3,4),(VRMediumRestr,2,3,4)]
  , [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,3,1,4),(VRMediumRestr,4,2,3)]
  , [(VRMediumRestr,1,2,3),(VRMediumRestr,2,1,4),(VRMediumRestr,4,1,3),(VRMediumRestr,3,2,4)]
  ]
