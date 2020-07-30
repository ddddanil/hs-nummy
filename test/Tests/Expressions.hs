module Tests.Expressions (testExpressions) where

import Protolude hiding (second)
import Data.String (String)
import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ((-|), (#^), (#*), (#/), dimless_unit, mkQu, (%^), (%*), (%/), (%+), (%-), Quantity(..))
import Tests.Definitions as Def
import Tests.Parser (checkQu, checkParseExpr)

qu' = Quantity

testExpressions =
  localOption average_timeout $ testGroup "Expressions"
  [ testGroup "Conversions"
    [ checkParseExpr Succeed "1in in mm" $ qu' (25.4, milli -| meter)
    , checkParseExpr Succeed "1in in m" $ qu' (0.0254, meter)
    , checkParseExpr Succeed "3 km^2 in m^2" $ qu' (3000000, meter #^ 2)
    , checkParseExpr Succeed "7.2 km/h in m/s" $ qu' (2, meter #/ second)
    ]
  , testGroup "Addition and subtraction"
    -- Dimless
    [ checkParseExpr Succeed "5 + 4.4" $ qu' (9.4, dimless_unit)
    , checkParseExpr Succeed "2.2 - 3" $ qu' (-0.8, dimless_unit)
    -- Same unit
    , checkParseExpr Succeed "4m + 7m" $ qu' (11, meter)
    , checkParseExpr Succeed "7s - 9.1s" $ qu' (-2.1, second)
    -- Mixed unit
    , checkParseExpr Succeed "1m + 1km" $ qu' (1001, meter)
    , checkParseExpr Succeed "1km + 1m" $ qu' (1.001, kilo -| meter)
    , checkParseExpr Succeed "1km + 1m in m" $ qu' (1001, meter)
    , checkParseExpr Succeed "1m + 1ft" $ qu' (1.3048, meter)
    ]
  , testGroup "Multiplication and division"
    [ checkParseExpr Succeed "2 m * 3 kg"   $ qu' (6, meter #* kilo -| gram)
    , checkParseExpr Succeed "10 m / 5 s"   $ qu' (2, meter #/ second)
    , checkParseExpr Succeed "10m / 2s in km/h" $ qu' (18, kilo -| meter #/ hour)
    ]
  ]
