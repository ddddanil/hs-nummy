module Tests.Quantities (testQuantities) where

import Protolude hiding (second)
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
import Tests.Parser (checkQu, checkParseQu)


testQuantities =
  -- localOption average_timeout $
  testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ checkParseQu Succeed "10"    (10 %# dimless)
    , checkParseQu Succeed "7.5"   (15/2 %# dimless)
    , checkParseQu Succeed "999.2" (4996/5 %# dimless)
    -- Different allowed spacings
    , checkParseQu Succeed "5m"    (5 %# meter)
    , checkParseQu Succeed "6 m"   (6 %# meter)
    , checkParseQu Succeed "8(m)"  (8 %# meter)
    , checkParseQu Succeed "7 (m)" (7 %# meter)
    ]
  , testGroup "Prefixes"
    -- Dimensionless
    [ checkParseQu Succeed "4 km"   (4000 %# meter)
    , checkParseQu Succeed "12 km"  (12 %# kilo -| meter)
    , checkParseQu Succeed "9000 mm"  (9 %# meter)
    , checkParseQu Succeed "2000000 mg"  (2 %# kilo -| gram)
    ]
  , testGroup "Complex"
    [ checkParseQu Succeed "1.5m/s"      (3/2 %# meter #/ second)
    , checkParseQu Succeed "9.8 (m/s s)" (49/5 %# meter #/ (second #* second))
    ]
  ]
