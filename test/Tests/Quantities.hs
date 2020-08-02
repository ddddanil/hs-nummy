module Tests.Quantities (testQuantities) where

import Nummy.Prelude (($), (/))

import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ( (-|), (#^), (#*), (#/), (%#) )
import Nummy.Metrology.Definitions
import Tests.Definitions
import Tests.Parser (checkParseQu)


testQuantities :: TestTree
testQuantities =
  localOption average_timeout $
  testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ checkParseQu Equal "10"    (10 %# scalar_unit)
    , checkParseQu Equal "7.5"   (15/2 %# scalar_unit)
    , checkParseQu Equal "999.2" (4996/5 %# scalar_unit)
    -- Different allowed spacings
    , checkParseQu Equal "5m"    (5 %# meter)
    , checkParseQu Equal "6 m"   (6 %# meter)
    , checkParseQu Equal "8(m)"  (8 %# meter)
    , checkParseQu Equal "7 (m)" (7 %# meter)
    -- Illegal spaces
    , checkParseQu Fail "6 m "  (6 %# meter)
    , checkParseQu Fail " 5m"     (5 %# meter)
    , checkParseQu Fail " 8(m) "  (8 %# meter)
    , checkParseQu Fail "  7 (m)" (7 %# meter)
    ]
  , testGroup "Prefixes"
    -- Dimensionless
    [ checkParseQu Equal "4 km"   (4000 %# meter)
    , checkParseQu Equal "12 km"  (12 %# kilo -| meter)
    , checkParseQu Equal "9000 mm"  (9 %# meter)
    , checkParseQu Equal "2000000 mg"  (2 %# kilo -| gram)
    ]
  , testGroup "Complex"
    [ checkParseQu Equal "1.5m/s"      (3/2 %# meter #/ second)
    , checkParseQu Equal "9.8 (m/s s)" (49/5 %# meter #/ (second #* second))
    ]
  ]
