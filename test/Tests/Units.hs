module Tests.Units (testUnits) where

import Nummy.Prelude hiding (second, bit)

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
import Tests.Parser (checkUnit, checkParseUnit)

testUnits c =
  -- localOption average_timeout $
  testGroup "Units"
  [ testGroup "Base"
    [ checkParseUnit Succeed "m" (meter) c
    , checkParseUnit Succeed "s" (second) c
    , checkParseUnit Succeed "g" (gram) c
    , checkParseUnit Succeed "A" (ampere) c
    , checkParseUnit Succeed "K" (kelvin) c
    , checkParseUnit Succeed "bit" (bit) c
    ]
  , testGroup "Syntax"
    [ checkParseUnit Fail  ""                  (dimless) c
    -- illegal spaces
    , checkParseUnit Fail  " m"             (meter) c
    , checkParseUnit Fail  "m "             (meter) c
    , checkParseUnit Fail  " m "            (meter) c
    , checkParseUnit Fail  "   m   "        (meter) c
    ]
  , testGroup "Prefixes"
    [ checkParseUnit Succeed  "mm" (milli -| meter) c
    , checkParseUnit Succeed  "ms" (milli -| second) c
    , checkParseUnit Succeed  "kg" (kilo -| gram) c
    , checkParseUnit Succeed  "daPa" (deca -| pascal) c
    , checkParseUnit Succeed  "Tbyte" (tera -| byte) c
    , checkParseUnit Succeed  "meganewton" (mega -| newton) c
    , checkParseUnit Succeed  "mum" (micro -| meter) c
    , checkParseUnit Succeed  "ng" (nano -| gram) c
    ]
  , testGroup "Division"
    [ checkParseUnit Succeed  "m/s"  (meter #/ second) c
    , checkParseUnit Succeed  "km/h" (kilo -| meter #/ hour) c
    , checkParseUnit Succeed  "1/s"  (dimless #/ second) c
    , checkParseUnit Succeed  "1/s"  (hertz) c
    , checkParseUnit Fail     "2/km" (dimless #/ second) c
    -- Simplification
    , checkParseUnit Succeed  "m s/m" (second) c
    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ checkParseUnit Succeed  "m*kg" (meter #* kilo -| gram) c
    , checkParseUnit Succeed  "km*g" (kilo -| meter #* gram) c
    -- Commutative units
    , checkParseUnit Succeed  "m s" (meter #* second) c
    , checkParseUnit Succeed  "s m" (meter #* second) c
    , checkParseUnit Succeed  "m s" (second #* meter) c
    -- Same dimension
    , checkParseUnit Succeed  "m*m"   (meter #* meter) c
    , checkParseUnit Succeed  "km*m"  (kilo -| meter #* meter) c
    , checkParseUnit Succeed  "km*m"  (meter #* kilo -| meter) c
    ]
  , testGroup "Powers"
    [ checkParseUnit Succeed  "m^2"  (meter #^ 2) c
    , checkParseUnit Succeed  "km^2" (kilo -| meter #^ 2) c
    -- Simplification
    , checkParseUnit Succeed  "m^2/m"  (meter) c
    , checkParseUnit Succeed  "m/m"    (dimless) c
    ]
  ]
