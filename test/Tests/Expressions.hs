module Tests.Expressions (testExpressions) where

import Nummy.Prelude (($))

import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ( (-|), (#^), (#*), (#/), (%#) )
import Nummy.Metrology.Definitions
import Tests.Definitions
import Tests.Parser (checkParseExpr)


testExpressions :: TestTree
testExpressions =
  localOption average_timeout $
  testGroup "Expressions"
  [ testGroup "Conversions"
    [ checkParseExpr Equal "1in | mm" (25.4 %# milli -| meter)
    , checkParseExpr Equal "1in | m" (0.0254 %# meter)
    , checkParseExpr Equal "3 km^2 | m^2" (3000000 %# meter #^ 2)
    , checkParseExpr Equal "7.2 km/h | m/s" (2 %# meter #/ second)
    , checkParseExpr Equal "3 kb/min | bit/s" (50 %# bit #/ second)
    , checkParseExpr Equal "7 nm * 1000000 | mm"   (7 %# milli -| meter)
    ]
  , testGroup "Addition and subtraction"
    -- Scalar
    [ checkParseExpr Equal "5 + 4.4" (9.4 %# scalar_unit)
    , checkParseExpr Equal "2.2 - 3" (-0.8 %# scalar_unit)
    , checkParseExpr Equal "5+6"     (11 %# scalar_unit)
    -- Same unit
    , checkParseExpr Equal "4m + 7m" (11 %# meter)
    , checkParseExpr Equal "10kg-7kg" (3 %# kilo -| gram)
    , checkParseExpr Equal "7s - 9.1s" (-2.1 %# second)
    -- Mixed unit
    , checkParseExpr Equal "1m + 1km" (1001 %# meter)
    , checkParseExpr Equal "1km + 1m" (1.001 %# kilo -| meter)
    , checkParseExpr Equal "1km + 1m | m" (1001 %# meter)
    , checkParseExpr Equal "1m + 1ft" (1.3048 %# meter)
    -- Illegal operations
    , checkParseExpr Fail "1m + 1h" (2 %# meter)
    , checkParseExpr Fail "1 (kg/s^2) + 1 n/kg" (1.001 %# kilo -| meter)
    , checkParseExpr Fail "1km + 1m | Pa" (1001 %# meter)
    , checkParseExpr Fail "1min - 1m" (1.3048 %# meter)
    ]
  , testGroup "Multiplication and division"
    [ checkParseExpr Equal "2 m * 3 kg"   (6 %# meter #* kilo -| gram)
    , checkParseExpr Equal "10 m / 5 s"   (2 %# meter #/ second)
    , checkParseExpr Equal "10m / 2s | km/h" (18 %# kilo -| meter #/ hour)
    , checkParseExpr Equal "4m ^ 2 / 2m"  (8 %# meter)
    ]
  ]
