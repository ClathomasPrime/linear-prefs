module LinearProgram where

import Numeric.LinearProgramming
import Data.List
import Data.Maybe

import GeneralD
import SingleCrossing
import Util
import LinearPref

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
  simplex (trivialObjective nVars) (Sparse $ feas' diff singCross)
    [ i :&: (1,bound) | i <- [1..nVars] ]
  where nVars = 2 * length (outcomes singCross)

trivialObjective :: Int -> Optimization
trivialObjective nVars = Maximize . replicate nVars $ 0

-- fix the places of crossing at arbitrary angles
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

-- fix the x values arbitrarily and make the slopes in the right order
feas' :: Double -> SingleCrossingSpec Int -> [Bound [(Double, Int)]]
feas' diff singCross@(SingleCrossingSpec outcomes fixeds pivots) =
  let (xOrder, yOrder) = extremalPrefs singCross
      xRank i = fromIntegral $ length xOrder - fromJust (elemIndex i xOrder)

      yOrderConstrs = [ [1#i, (-1)#j] :>=: diff
        | (i,j) <- adjacentPairs yOrder]

      crossBeforeConstr i j i' j' =
        [ (ifEq i alpha + ifEq j (-alpha) + ifEq i' (-beta) + ifEq j' (beta)) # u
          | u <- nub [i,j,i',j'], let ifEq x v = if x == u then v else 0
        ] :<=: (-diff)
        where alpha = 1 / (xRank i - xRank j)
              beta = 1 / (xRank i' - xRank j')
        -- | i == i' = [ (alpha - )#i , (-alpha)#j, (-beta)#i' , (beta)#j' ] :>=: 0
        -- | j == j' =
        -- | otherwise = [ (alpha)#i , (-alpha)#j, (-beta)#i' , (beta)#j' ] :>=: 0
      crossingConstrs = [ crossBeforeConstr i j i' j'
        | (piv1, piv2) <- adjacentPairs pivots
        , (i,j) <- piv1, (i',j') <- piv2 ]

   in yOrderConstrs ++ crossingConstrs

-- fix the x values arbitrarily and make the slopes in the right order
feas'' :: Double -> SingleCrossingSpec Int -> [Bound [(Double, Int)]]
feas'' diff singCross@(SingleCrossingSpec outcomes fixeds pivots) =
  let (xOrder, yOrder) = extremalPrefs singCross
      -- xRank i = fromIntegral $ length xOrder - fromJust (elemIndex i xOrder)

      xOrderConstrs = [ [1#i, (-1)#j] :>=: diff
        | (i,j) <- adjacentPairs xOrder]

      pad i = length outcomes + i
      yOrderConstrs = [ [1#pad i, (-1)#pad j] :>=: diff
        | (i,j) <- adjacentPairs yOrder]

      nCrosses = fromIntegral . length . pivotList $ singCross
      crossPoints :: [Double]
      crossPoints = fmap (/(nCrosses+1)) [1,2..nCrosses]
      meetAtPoint i j t =
        [ (1-t)#i, t#j, (t-1)#pad i, (-t)#pad j] :==: 0
        -- [(cos theta)#i, (-cos theta)#j,
        --   (sin theta)#pad i, (-sin theta)#pad j] :==: 0
      crossingConstrs = [ meetAtPoint i j t
        | (pairs, t) <- zip (pivotList singCross) crossPoints
        , (i,j) <- pairs ]

   in xOrderConstrs ++ yOrderConstrs ++ crossingConstrs

experimentFeasFixed :: IO (Maybe (SingleCrossingSpec Int))
experimentFeasFixed = tryUntil 10 (not . null . outcomes) $ do
  let nOutcomes = 4
      diff = 0.1
      bound = 10000000
  prefSpec <- randSingCrossing nOutcomes
  let res = runLp bound diff prefSpec
  traceShowM (specToPrefs prefSpec)
  traceShowM res
  if show res == "NoFeasible"
    then do
      r <- experimentFindSpecific prefSpec
      case r of
        Nothing -> return prefSpec
        Just _ -> return $ SingleCrossingSpec [] [] []
    else return $ SingleCrossingSpec [] [] []

experimentFindSpecific :: SingleCrossingSpec Int -> IO (Maybe [(Int, [Double])])
experimentFindSpecific spec = tryUntilSomething 5000 $ do
  -- prefs <- randomFullPrefs 2 4 0.001
  points <- arbitraryLable <$> unitBoxPoints 2 4
  let prefs = genLinearPrefs 2 0.001 points
  if specToPrefs spec `eqAsSet` prefs
    then return $ Just points
    else return Nothing

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
heresOne = (SingleCrossingSpec {outcomes = [1,2,3,4,5], fixedPairs = [(1,2),(1,4),(1,5),(3,4),(3,5)], pivotList = [[(4,5)],[(2,3)],[(1,3)],[(2,5)],[(2,4)]]})
