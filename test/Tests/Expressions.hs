module Tests.Expressions (testExpressions) where

import Nummy.Prelude hiding (second, bit)

import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology (Quantity(..), Unit, Dimension, Prefix
                       , (|^|), (|*|), (|/|)
                       , (-|), (#^), (#*), (#/)
                       , (%#), (%<|), (%^), (%*), (%/), (%+), (%-)
                       )
import Nummy.Metrology.Definitions
import Tests.Definitions
import Tests.Parser (checkQu, checkParseExpr)


testExpressions c =
  -- localOption average_timeout $
  testGroup "Expressions"
  [ testGroup "Conversions"
    [ checkParseExpr Succeed "1in | mm" (25.4 %# milli -| meter) c
    , checkParseExpr Succeed "1in | m" (0.0254 %# meter) c
    , checkParseExpr Succeed "3 km^2 | m^2" (3000000 %# meter #^ 2) c
    , checkParseExpr Succeed "7.2 km/h | m/s" (2 %# meter #/ second) c
    , checkParseExpr Succeed "3 kbit/min | bit/s" (50 %# bit #/ second) c
    , checkParseExpr Succeed "7 nm * 1000000 | mm"   (7 %# milli -| meter) c
    ]
  , testGroup "Addition and subtraction"
    -- Dimless
    [ checkParseExpr Succeed "5 + 4.4" (9.4 %# dimless) c
    , checkParseExpr Succeed "2.2 - 3" (-0.8 %# dimless) c
    -- Same unit
    , checkParseExpr Succeed "4m + 7m" (11 %# meter) c
    , checkParseExpr Succeed "7s - 9.1s" (-2.1 %# second) c
    -- Mixed unit
    , checkParseExpr Succeed "1m + 1km" (1001 %# meter) c
    , checkParseExpr Succeed "1km + 1m" (1.001 %# kilo -| meter) c
    , checkParseExpr Succeed "1km + 1m | m" (1001 %# meter) c
    , checkParseExpr Succeed "1m + 1ft" (1.3048 %# meter) c
    ]
  , testGroup "Multiplication and division"
    [ checkParseExpr Succeed "2 m * 3 kg"   (6 %# meter #* kilo -| gram) c
    , checkParseExpr Succeed "10 m / 5 s"   (2 %# meter #/ second) c
    , checkParseExpr Succeed "10m / 2s | km/h" (18 %# kilo -| meter #/ hour) c
    , checkParseExpr Succeed "4m ^ 2 / 2m"  (8 %# meter) c
    ]
  ]
