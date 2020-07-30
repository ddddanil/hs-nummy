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

testUnits =
  -- localOption average_timeout $
  testGroup "Units"
  [ testGroup "Base"
    [ checkParseUnit Succeed "m" (meter)
    , checkParseUnit Succeed "s" (second)
    , checkParseUnit Succeed "g" (gram)
    , checkParseUnit Succeed "A" (ampere)
    , checkParseUnit Succeed "K" (kelvin)
    , checkParseUnit Succeed "bit" (bit)
    ]
  , testGroup "Syntax"
    [ checkParseUnit Fail  ""                  (dimless)
    -- illegal spaces
    , checkParseUnit Fail  " m"             (meter)
    , checkParseUnit Fail  "m "             (meter)
    , checkParseUnit Fail  " m "            (meter)
    , checkParseUnit Fail  "   m   "        (meter)
    ]
  , testGroup "Prefixes"
    [ checkParseUnit Succeed  "mm" (milli -| meter)
    , checkParseUnit Succeed  "ms" (milli -| second)
    , checkParseUnit Succeed  "kg" (kilo -| gram)
    , checkParseUnit Succeed  "daPa" (deca -| pascal)
    , checkParseUnit Succeed  "Tbyte" (tera -| byte)
    , checkParseUnit Succeed  "meganewton" (mega -| newton)
    , checkParseUnit Succeed  "mum" (micro -| meter)
    , checkParseUnit Succeed  "ng" (nano -| gram)
    ]
  , testGroup "Division"
    [ checkParseUnit Succeed  "m/s"  (meter #/ second)
    , checkParseUnit Succeed  "km/h" (kilo -| meter #/ hour)
    , checkParseUnit Succeed  "1/s"  (dimless #/ second)
    , checkParseUnit Succeed  "1/s"  (hertz)
    , checkParseUnit Fail     "2/km" (dimless #/ second)
    -- Simplification
    , checkParseUnit Succeed  "m s/m" (second)
    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ checkParseUnit Succeed  "m*kg" (meter #* kilo -| gram)
    , checkParseUnit Succeed  "km*g" (kilo -| meter #* gram)
    -- Commutative units
    , checkParseUnit Succeed  "m s" (meter #* second)
    , checkParseUnit Succeed  "s m" (meter #* second)
    , checkParseUnit Succeed  "m s" (second #* meter)
    -- Same dimension
    , checkParseUnit Succeed  "m*m"   (meter #* meter)
    , checkParseUnit Succeed  "km*m"  (kilo -| meter #* meter)
    , checkParseUnit Succeed  "km*m"  (meter #* kilo -| meter)
    -- Illegal wide notation
    , checkParseUnit Fail     "m s"   (meter #* second)
    ]
  , testGroup "Powers"
    [ checkParseUnit Succeed  "m^2"  (meter #^ 2)
    , checkParseUnit Succeed  "km^2" (kilo -| meter #^ 2)
    -- Simplification
    , checkParseUnit Succeed  "m^2/m"  (meter)
    , checkParseUnit Succeed  "m/m"    (dimless)
    ]
  ]
