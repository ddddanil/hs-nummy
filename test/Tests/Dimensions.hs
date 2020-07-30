module Tests.Dimensions (testDimensions) where

import Protolude hiding (length)
import Data.String (String)
import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology (Quantity(..), Unit, Dimension, Prefix
                       , (|^|), (|*|), (|/|)
                       , (-|), (#^), (#*), (#/)
                       , (%#), (%<|), (%^), (%*), (%/), (%+), (%-)
                       )
import Nummy.Metrology.Definitions
import Nummy.Metrology.Definitions.Dimension
import Tests.Definitions
import Tests.Parser (checkDim)


testDimensions =
  testGroup "Dimensions"
  [ checkDim Succeed "len * len == len ^ 2" (length |*| length) (length |^| 2)
  , checkDim Succeed "len * time / time == len" (length |*| time |/| time) (length)
  , checkDim Succeed "time * len / time == len" (time |*| length |/| time) (length)
  , checkDim Succeed "len ^2 / len == len" (length |^| 2 |/| length) (length)
  ]
