module Tests.Expressions (testExpressions) where

import Protolude hiding (second)
import Data.String (String)
import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ((-|), (#^), (#*), (#/), (%#), (%<|), (%^), (%*), (%/), (%+), (%-), Quantity(..))
import Tests.Definitions
import Nummy.Metrology.Definitions as Def
import Tests.Parser (checkQu, checkParseExpr)


testExpressions =
  localOption average_timeout $ testGroup "Expressions"
  [ testGroup "Conversions"
    [ checkParseExpr Succeed "1in in mm" (25.4 %# milli -| meter)
    , checkParseExpr Succeed "1in in m" (0.0254 %# meter)
    , checkParseExpr Succeed "3 km^2 in m^2" (3000000 %# meter #^ 2)
    , checkParseExpr Succeed "7.2 km/h in m/s" (2 %# meter #/ second)
    ]
  , testGroup "Addition and subtraction"
    -- Dimless
    [ checkParseExpr Succeed "5 + 4.4" (9.4 %# dimless_unit)
    , checkParseExpr Succeed "2.2 - 3" (-0.8 %# dimless_unit)
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
