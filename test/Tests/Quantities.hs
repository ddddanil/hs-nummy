module Tests.Quantities (testQuantities) where

import Nummy.Prelude (($), (/))

import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ( (-|), (#^), (#*), (#/), (%#) )
import Nummy.Metrology.Definitions
import Nummy.Cache (CurrencyCache)
import Tests.Definitions
import Tests.Parser (checkParseQu)


testQuantities :: CurrencyCache -> TestTree
testQuantities c =
  localOption average_timeout $
  testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ checkParseQu Succeed "10"    (10 %# scalar_unit) c
    , checkParseQu Succeed "7.5"   (15/2 %# scalar_unit) c
    , checkParseQu Succeed "999.2" (4996/5 %# scalar_unit) c
    -- Different allowed spacings
    , checkParseQu Succeed "5m"    (5 %# meter) c
    , checkParseQu Succeed "6 m"   (6 %# meter) c
    , checkParseQu Succeed "8(m)"  (8 %# meter) c
    , checkParseQu Succeed "7 (m)" (7 %# meter) c
    -- Illegal spaces
    , checkParseQu Fail "6 m "  (6 %# meter) c
    , checkParseQu Fail " 5m"     (5 %# meter) c
    , checkParseQu Fail " 8(m) "  (8 %# meter) c
    , checkParseQu Fail "  7 (m)" (7 %# meter) c
    ]
  , testGroup "Prefixes"
    -- Dimensionless
    [ checkParseQu Succeed "4 km"   (4000 %# meter) c
    , checkParseQu Succeed "12 km"  (12 %# kilo -| meter) c
    , checkParseQu Succeed "9000 mm"  (9 %# meter) c
    , checkParseQu Succeed "2000000 mg"  (2 %# kilo -| gram) c
    ]
  , testGroup "Complex"
    [ checkParseQu Succeed "1.5m/s"      (3/2 %# meter #/ second) c
    , checkParseQu Succeed "9.8 (m/s s)" (49/5 %# meter #/ (second #* second)) c
    ]
  ]
