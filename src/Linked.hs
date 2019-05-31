module Linked where

-- import Data.List
-- import Data.Maybe
-- import Control.Monad

-- import GeneralD
-- import Util
-- import LinearPref
import Hypergraph as H


-- non-linked.
squareLinkages :: [[Int]]
squareLinkages = H.maxPrefSet [1..4] gr
  where gr = [ ([1,3], [2,4]), ([2,4], [1,3]) ]

squareLinksNondict :: [[Int]]
squareLinksNondict =
  [ [1,2,4,3]
  , [2,1,3,4]
  , [2,3,1,4]
  , [3,2,4,1]
  , [3,4,2,1]
  , [4,3,1,2]
  , [4,1,3,2]
  , [1,4,2,3]
  ]
