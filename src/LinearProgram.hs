module LinearProgram where

import Numeric.LinearProgramming
import Data.List
import Data.Maybe

import GeneralD
import SingleCrossing
import Util
import LinearPref

prob :: Optimization
prob = Maximize [4, -3, 2]

constr1 :: Constraints
constr1 = Sparse [ [2#1, 1#2] :<=: 10
                 , [1#2, 5#3] :<=: 20
                 , [2#1, 1#2] :>=: 20
                 ]

type ConstraintGenerator = Double -> SingleCrossingSpec Int
  -> [Bound [(Double, Int)]]

runLp :: ConstraintGenerator -> Double -> Double
  -> SingleCrossingSpec Int -> Solution
runLp feas bound diff singCross =
  simplex (trivialObjective nVars) (Sparse $ feas diff singCross)
    [ i :&: (1,bound) | i <- [1..nVars] ]
  where nVars = 2 * length (scOutcomes singCross)

trivialObjective :: Int -> Optimization
trivialObjective nVars = Maximize . replicate nVars $ 0

-- fix the places of crossing at arbitrary angles
feasFixCrossAngle :: Double -> SingleCrossingSpec Int -> [Bound [(Double, Int)]]
feasFixCrossAngle diff singCross =
  let (xOrder, yOrder) = extremalPrefs singCross
      xOrderConstrs = [ [1#i, (-1)#j] :>=: diff
        | (i,j) <- adjacentPairs xOrder]
      pad i = length (scOutcomes singCross) + i
      yOrderConstrs = [ [1#pad i, (-1)#pad j] :>=: diff
        | (i,j) <- adjacentPairs yOrder]
      nCrosses = fromIntegral . length . scPivotList $ singCross
      angles :: [Double]
      angles = fmap (* (pi / 2 / (nCrosses + 1))) [1,2..nCrosses]
      meetAtAngleConstr i j theta =
        [(cos theta)#i, (-cos theta)#j,
          (sin theta)#pad i, (-sin theta)#pad j] :==: 0
      crossingConstrs = [ meetAtAngleConstr i j theta
        | (pairs, theta) <- zip (scPivotList singCross) angles
        , (i,j) <- pairs ]
   in xOrderConstrs ++ yOrderConstrs ++ crossingConstrs

-- fix the x values arbitrarily and make the slopes in the right order
feasXFixed :: Double -> SingleCrossingSpec Int -> [Bound [(Double, Int)]]
feasXFixed diff singCross@(SingleCrossingSpec _ _ pivots) =
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

-- fix the slope crossing points at arbitrary time steps
feasSlopeVals :: Double -> SingleCrossingSpec Int -> [Bound [(Double, Int)]]
feasSlopeVals diff singCross@(SingleCrossingSpec outcomes _ pivots) =
  let (xOrder, yOrder) = extremalPrefs singCross
      -- xRank i = fromIntegral $ length xOrder - fromJust (elemIndex i xOrder)

      xOrderConstrs = [ [1#i, (-1)#j] :>=: diff
        | (i,j) <- adjacentPairs xOrder]

      pad i = length outcomes + i
      yOrderConstrs = [ [1#pad i, (-1)#pad j] :>=: diff
        | (i,j) <- adjacentPairs yOrder]

      nCrosses = fromIntegral . length $ pivots
      crossPoints :: [Double]
      crossPoints = fmap (/(nCrosses+1)) [1,2..nCrosses]
      meetAtPoint i j t =
        [ (1-t)#i, t#j, (t-1)#pad i, (-t)#pad j] :==: 0
        -- [(cos theta)#i, (-cos theta)#j,
        --   (sin theta)#pad i, (-sin theta)#pad j] :==: 0
      crossingConstrs = [ meetAtPoint i j t
        | (pairs, t) <- zip pivots crossPoints
        , (i,j) <- pairs ]

   in xOrderConstrs ++ yOrderConstrs ++ crossingConstrs

-- fix the voters but not the exact cross points
feasPrefAngle :: Double -> SingleCrossingSpec Int -> [Bound [(Double, Int)]]
feasPrefAngle diff singCross@(SingleCrossingSpec outcomes _ pivots) =
  let (xOrder, yOrder) = extremalPrefs singCross
      xOrderConstrs = [ [1#i, (-1)#j] :>=: diff
        | (i,j) <- adjacentPairs xOrder]
      pad i = length outcomes + i
      yOrderConstrs = [ [1#pad i, (-1)#pad j] :>=: diff
        | (i,j) <- adjacentPairs yOrder]
      --- NOTE: I might not actually want these

      nPrefs = 1 + (fromIntegral . length $ pivots)
      prefs = specToPrefs singCross
      angles :: [Double]
      angles = fmap (* (pi / 2 / (nPrefs + 1))) [1,2..nPrefs]

      prefAtAngleConstrs pref theta =
        [ [ (cos theta)#i, (-cos theta)#j,
          (sin theta)#pad i, (-sin theta)#pad j] :>=: diff
          | (i, j) <- adjacentPairs pref ]

      prefConstrs = [ constr
        | (pref, theta) <- zip prefs angles
        , constr <- prefAtAngleConstrs pref theta]
   in xOrderConstrs ++ yOrderConstrs ++ prefConstrs

experimentFeasFixed :: IO (Maybe (SingleCrossingSpec Int))
experimentFeasFixed = tryUntil 500 (not . null . scOutcomes) $ do
  let nOutcomes = 6
      diff = 1
      bound = 10000000
  prefSpec <- randSingCrossingBiased 0.4 nOutcomes
  let res = runLp feasPrefAngle bound diff prefSpec
  -- traceShowM (specToPrefs prefSpec)
  -- traceShowM res
  if show res == "NoFeasible"
    then do
      r <- experimentFindSpecific prefSpec
      case r of
        Nothing -> return prefSpec
        Just _ -> return $ SingleCrossingSpec [] [] []
    else return $ SingleCrossingSpec [] [] []

experimentFindSpecific :: SingleCrossingSpec Int -> IO (Maybe [(Int, [Double])])
experimentFindSpecific spec = tryUntilSomething 50 $ do
  -- prefs <- randomFullPrefs 2 4 0.001
  points <- arbitraryLable <$> unitBoxPoints 2 4
  let prefs = genLinearPrefs 2 0.001 points
  if specToPrefs spec `eqAsSet` prefs
    then return $ Just points
    else return Nothing
