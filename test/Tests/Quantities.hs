module Tests.Quantities (testQuantities) where

import Protolude hiding (second)
import Data.String (String)
import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ((-|), (#^), (#*), (#/), dimless_unit, mkQu, (%^), (%*), (%/), (%+), (%-), Quantity(..))
import Tests.Definitions as Def
import Tests.Parser (checkQu, checkParseQu)

qu' = Quantity

testQuantities =
  localOption average_timeout $ testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ checkParseQu Succeed "10"    $ qu' (10, dimless_unit)
    , checkParseQu Succeed "7.5"   $ qu' (15/2, dimless_unit)
    , checkParseQu Succeed "999.2" $ qu' (4996/5, dimless_unit)
    -- Different allowed spacings
    , checkParseQu Succeed "5m"    $ qu' (5, meter)
    , checkParseQu Succeed "6 m"   $ qu' (6, meter)
    , checkParseQu Succeed "8(m)"  $ qu' (8, meter)
    , checkParseQu Succeed "7 (m)" $ qu' (7, meter)
    ]
  , testGroup "Modifiers and prefixes"
    -- Dimensionless
    [ checkParseQu Succeed "6k"     $ qu' (6000, dimless_unit)
    -- Commutativity of modifiers and prefixes
    , checkParseQu Succeed "3k m"   $ qu' (3000, meter)
    , checkParseQu Succeed "4 km"   $ qu' (4000, meter)
    , checkParseQu Succeed "12 km"  $ qu' (12, kilo -| meter)
    , checkParseQu Succeed "11k km" $ qu' (11000000, meter)
    , checkParseQu Succeed "9k mm"  $ qu' (9, meter)
    , checkParseQu Succeed "2m mg"  $ qu' (2, kilo -| gram)
    ]
  , testGroup "Complex"
    [ checkParseQu Succeed "1.5m/s"      $ qu' (3/2, meter #/ second)
    , checkParseQu Succeed "9.8 (m/s s)" $ qu' (49/5, meter #/ (second #* second))
    ]
  ]
