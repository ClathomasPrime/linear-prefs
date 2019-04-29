{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, NoMonomorphismRestriction #-}
module Plotting.SimplexPartition
  ( simplexMain
  ) where


import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Canvas.CmdLine
import Data.List

import Debug.Trace as Tr

type Candidate = (Double, Double, Double)

simplexMain :: IO ()
simplexMain = mainWith
  -- ((square (1::Double) # translate (r2 (0.5,0.5)) # bg white) :: Diagram B)
  -- `atop`
  ((fromVertices (map p2 [(0, 1), (1, 0)]) # lwG 0.02)
    `atop` plotAllIndifferences candidates)

candidates :: [Candidate]
candidates =
  -- [(0,0,1), (0,1,0), (1,0,0), (0.25,0.25,0.25)] -- (0.7,0.7,0.7)]
  [ (1,   0,   0)
  , (0.5, 0.9, 1)
  , (0,   1,   0)
  , (0.9, 0.5, 1)
  ]

plotAllIndifferences :: [Candidate] -> Diagram B
plotAllIndifferences
  = foldl atop mempty . fmap (uncurry plotIndifference) . traceShowId . pairs

pairs :: [a] -> [(a,a)]
pairs xs = [(a,b) | a:bs <- tails xs, b <- bs]

plotIndifference :: Candidate -> Candidate -> Diagram B
plotIndifference (a1,a2,a3) (b1,b2,b3) = lineWithCoefs u v c
  where u = a1 - b1 + b3 - a3
        v = a2 - b2 + b3 - a3
        c = b3 - a3


-- u x + v y = c
lineWithCoefs :: Double -> Double -> Double -> Diagram B
lineWithCoefs u v c =
  let xInt = (0, c / u)
      yInt = (c / v, 0)
      diagInt = ( (c-v)/(u-v) , (c-u)/(v-u) )
      val (x,y) = all (\w -> 0 <= w && w <= 1) [x,y]
      valids = filter val [xInt, yInt, diagInt]
   in if length valids >= 2
         then fromVertices (map p2 $ traceShowId valids) # lwG 0.005
         else traceShow (u,v,c) mempty


-- LOL this had nothing to do with anything.
-- plotEquidist :: Candidate -> Candidate -> Diagram B
-- plotEquidist (a1,a2,a3) (b1,b2,b3) = lineWithCoefs u v c
--   where u = b1 - a1 - b3 + a3
--         v = b2 - a2 - b3 + a3
--         c = a3 - b3 + ssqdif / 2
--         ssqdif = (b1^2 +b2^2 +b3^2) - (a1^2 +a2^2 +a3^2)
--
