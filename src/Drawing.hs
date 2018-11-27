{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Drawing
  ( main'
  ) where

import Diagrams.Prelude hiding (R2, R3)
-- import qualified Diagrams.Prelude as D
import Diagrams.Backend.SVG.CmdLine

import BoundedD

{-
mainRec :: IO ()
mainRec = do
  ps <- arbitraryLable <$> replicateM 5 pointsToTenthInUnitBox
  let rankings = record (length . tightSweep) ps
  print $ rankings

  return ()
-}

--------------------------------------------------------------------------------

prefPointsDrawing :: [(Int, R2)] -> Diagram B
prefPointsDrawing prefSet =
  (mconcat . fmap phi $ prefSet)
  <> (square 1 # translate (r2 (0.5,0.5)) # bg white)
  where phi (i, xy) =
          hsep 0.03 [text (show i) # scale 0.05, circle 0.005]
          # translate (r2 xy)

main' :: IO ()
main' =
  mainWith (prefPointsDrawing $ interiorCirclePrefPoints 5 3)
  -- mainWith (prefPointsDrawing maximalWithThree')
  -- mainWith (prefPointsDrawing complexPrefPoints)
  -- mainWith $ do
  --   let phi (n, ps) = hsep 0.1
  --         [text (show n) # scale 0.1, prefPointsDrawing ps]
  --   hsep 0.5 . fmap phi $ manyPrefPoints


--------------------------------------------------------------------------------
