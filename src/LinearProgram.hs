module LinearProgram where

import Numeric.LinearProgramming

import SingleCrossing
import Util
import Debug.Trace

prob :: Optimization
prob = Maximize [4, -3, 2]

constr1 :: Constraints
constr1 = Sparse [ [2#1, 1#2] :<=: 10
                 , [1#2, 5#3] :<=: 20
                 , [2#1, 1#2] :>=: 20
                 ]

runLp :: Double -> Double -> SingleCrossingSpec Int -> Solution
runLp bound diff singCross =
  simplex (trivialObjective nVars) (Sparse $ feas diff singCross)
    [ i :&: (1,bound) | i <- [1..nVars] ]
  where nVars = 2 * length (outcomes singCross)

trivialObjective :: Int -> Optimization
trivialObjective nVars = Maximize . replicate nVars $ 0

feas :: Double -> SingleCrossingSpec Int -> [Bound [(Double, Int)]]
feas diff singCross =
  let (xOrder, yOrder) = extremalPrefs singCross
      xOrderConstrs = [ [1#i, (-1)#j] :>=: diff
        | (i,j) <- adjacentPairs xOrder]
      pad i = length (outcomes singCross) + i
      yOrderConstrs = [ [1#pad i, (-1)#pad j] :>=: diff
        | (i,j) <- adjacentPairs yOrder]
      nCrosses = fromIntegral . length . pivotList $ singCross
      angles :: [Double]
      angles = fmap (* (pi / 2 / (nCrosses + 1))) [1,2..nCrosses]
      meetAtAngleConstr i j theta =
        [(cos theta)#i, (-cos theta)#j,
          (sin theta)#pad i, (-sin theta)#pad j] :==: 0
      crossingConstrs = [ meetAtAngleConstr i j theta
        | (pairs, theta) <- zip (pivotList singCross) angles
        , (i,j) <- pairs ]
   in xOrderConstrs ++ yOrderConstrs ++ crossingConstrs

experimento :: IO (Maybe (SingleCrossingSpec Int))
experimento = tryUntil 10 (not . null . outcomes) $ do
  let nOutcomes = 10
      diff = 1.0
      bound = 1000
  prefSpec <- randSingCrossing nOutcomes
  let res = runLp bound diff prefSpec
  traceShowM res
  traceShowM (specToPrefs prefSpec)
  return $ if show res == "NoFeasible"
    then SingleCrossingSpec [] [] []
    else prefSpec


--------------------------------------------------------------------------------

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
