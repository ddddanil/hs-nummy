module Tests.Expressions (testExpressions) where

import Protolude hiding (second, bit)
import Data.String (String)
import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology (Quantity(..), Unit, Dimension, Prefix
                       , (|^|), (|*|), (|/|)
                       , (-|), (#^), (#*), (#/)
                       , (%#), (%<|), (%^), (%*), (%/), (%+), (%-)
                       )
import Nummy.Metrology.Definitions
import Nummy.Metrology.Definitions.Unit
import Nummy.Metrology.Definitions.Prefix
import Tests.Definitions
import Tests.Parser (checkQu, checkParseExpr)


testExpressions =
  -- localOption average_timeout $
  testGroup "Expressions"
  [ testGroup "Conversions"
    [ checkParseExpr Succeed "1in in mm" (25.4 %# milli -| meter)
    , checkParseExpr Succeed "1in in m" (0.0254 %# meter)
    , checkParseExpr Succeed "3 km^2 in m^2" (3000000 %# meter #^ 2)
    , checkParseExpr Succeed "7.2 km/h in m/s" (2 %# meter #/ second)
    , checkParseExpr Succeed "3 kbit/min in bit/s" (50 %# bit #/ second)
    , checkParseExpr Succeed "7 nm * 1000000 to mm"   (7 %# milli -| meter)
    , checkParseExpr Succeed "7 nm * 1000000 into mm"   (7 %# milli -| meter)
    , checkParseExpr Succeed "7 nm * 1000000 in mm"   (7 %# milli -| meter)
    ]
  , testGroup "Addition and subtraction"
    -- Dimless
    [ checkParseExpr Succeed "5 + 4.4" (9.4 %# dimless)
    , checkParseExpr Succeed "2.2 - 3" (-0.8 %# dimless)
    -- Same unit
    , checkParseExpr Succeed "4m + 7m" (11 %# meter)
    , checkParseExpr Succeed "7s - 9.1s" (-2.1 %# second)
    -- Mixed unit
    , checkParseExpr Succeed "1m + 1km" (1001 %# meter)
    , checkParseExpr Succeed "1km + 1m" (1.001 %# kilo -| meter)
    , checkParseExpr Succeed "1km + 1m in m" (1001 %# meter)
    , checkParseExpr Succeed "1m + 1ft" (1.3048 %# meter)
    ]
  , testGroup "Multiplication and division"
    [ checkParseExpr Succeed "2 m * 3 kg"   (6 %# meter #* kilo -| gram)
    , checkParseExpr Succeed "10 m / 5 s"   (2 %# meter #/ second)
    , checkParseExpr Succeed "10m / 2s in km/h" (18 %# kilo -| meter #/ hour)
    ]
  ]
