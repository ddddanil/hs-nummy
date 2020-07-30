module Tests.Dimensions (testDimensions) where

import Protolude hiding (length)
import Data.String (String)
import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ((|^|), (|*|), (|/|), dimless, length, time, mass, current, temp)
import Tests.Definitions as Def
import Tests.Parser (checkDim)


testDimensions =
  testGroup "Dimensions"
  [ checkDim Succeed "len * len = len ^ 2" (length |*| length) (length |^| 2)
  ]
