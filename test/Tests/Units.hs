module Tests.Units (testUnits) where

import Nummy.Prelude (($))

import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ((-|), (#^), (#*), (#/))
import Nummy.Metrology.Definitions
import Tests.Definitions
import Tests.Parser (checkParseUnit)


testUnits :: TestTree
testUnits =
  localOption average_timeout $
  testGroup "Units"
  [ testGroup "Base"
    [ checkParseUnit Equal "m" (meter)
    , checkParseUnit Equal "s" (second)
    , checkParseUnit Equal "g" (gram)
    , checkParseUnit Equal "A" (ampere)
    , checkParseUnit Equal "K" (kelvin)
    , checkParseUnit Equal "b" (bit)
    ]
  , testGroup "Syntax"
    [ checkParseUnit Fail  ""        (scalar_unit)
    -- illegal spaces
    , checkParseUnit Fail  " m"      (meter)
    , checkParseUnit Fail  "m "      (meter)
    , checkParseUnit Fail  " m "     (meter)
    , checkParseUnit Fail  "   m   " (meter)
    ]
  , testGroup "Prefixes"
    [ checkParseUnit Equal  "mm" (milli -| meter)
    , checkParseUnit Equal  "ms" (milli -| second)
    , checkParseUnit Equal  "kg" (kilo -| gram)
    , checkParseUnit Equal  "daPa" (deca -| pascal)
    , checkParseUnit Equal  "terabyte" (tera -| byte)
    , checkParseUnit Equal  "TB" (tera -| byte)
    , checkParseUnit Equal  "meganewton" (mega -| newton)
    , checkParseUnit Equal  "mum" (micro -| meter)
    , checkParseUnit Equal  "ng" (nano -| gram)
    ]
  , testGroup "Division"
    [ checkParseUnit Equal  "m/s"  (meter #/ second)
    , checkParseUnit Equal  "km/h" (kilo -| meter #/ hour)
    , checkParseUnit Equal  "1/s"  (scalar_unit #/ second)
    , checkParseUnit Equal  "1/s"  (hertz)
    , checkParseUnit Fail     "2/km" (scalar_unit #/ second)
    -- Simplification
    , checkParseUnit Equal  "m s/m" (second)    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ checkParseUnit Equal  "m*kg" (meter #* kilo -| gram)
    , checkParseUnit Equal  "km*g" (kilo -| meter #* gram)
    -- Commutative units
    , checkParseUnit Equal  "m s" (meter #* second)
    , checkParseUnit Equal  "s m" (meter #* second)
    , checkParseUnit Equal  "m s" (second #* meter)
    -- Same dimension
    , checkParseUnit Equal  "m*m"   (meter #* meter)
    , checkParseUnit Equal  "km*m"  (kilo -| meter #* meter)
    , checkParseUnit Equal  "km*m"  (meter #* kilo -| meter)
    ]
  , testGroup "Powers"
    [ checkParseUnit Equal  "m^2"  (meter #^ 2)
    , checkParseUnit Equal  "km^2" (kilo -| meter #^ 2)
    -- Simplification
    , checkParseUnit Equal  "m^2/m"  (meter)
    , checkParseUnit Equal  "m/m"    (scalar_unit)
    ]
  ]
