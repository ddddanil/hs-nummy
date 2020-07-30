module Tests.Units (testUnits) where

import Protolude hiding (second)
import Data.String (String)
import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ((-|), (#^), (#*), (#/))
import Tests.Definitions
import Nummy.Metrology.Definitions as Def
import Tests.Parser (checkUnit, checkParseUnit)

testUnits =
  localOption average_timeout $ testGroup "Units"
  [ testGroup "Base"
    [ checkParseUnit Succeed "m" (meter)
    , checkParseUnit Succeed "s" (second)
    , checkParseUnit Succeed "g" (gram)
    , checkParseUnit Succeed "A" (ampere)
    , checkParseUnit Succeed "K" (kelvin)
    ]
  , testGroup "Syntax"
    [ checkParseUnit Fail  ""                  (dimless_unit)
    -- optional parenthesis
    , checkParseUnit Succeed  "(m)"            (meter)
    , checkParseUnit Succeed  "( m )"          (meter)
    , checkParseUnit Succeed  "(    m       )" (meter)
    -- illegal spaces
    , checkParseUnit Fail  " m"             (meter)
    , checkParseUnit Fail  "m "             (meter)
    , checkParseUnit Fail  " m "            (meter)
    , checkParseUnit Fail  "   m   "        (meter)
    , checkParseUnit Fail  " (m)"           (meter)
    , checkParseUnit Fail  "(m) "           (meter)
    , checkParseUnit Fail  " (m) "          (meter)
    , checkParseUnit Fail  "  (m)    "      (meter)
    ]
  , testGroup "Prefixes"
    [ checkParseUnit Succeed  "mm" (milli -| meter)
    , checkParseUnit Succeed  "ms" (milli -| second)
    , checkParseUnit Succeed  "kg" (kilo -| gram)
    ]
  , testGroup "Division"
    [ checkParseUnit Succeed  "m/s"  (meter #/ second)
    , checkParseUnit Succeed  "km/h" (kilo -| meter #/ hour)
    , checkParseUnit Succeed  "1/s"  (dimless_unit #/ second)
    , checkParseUnit Fail     "2/km" (dimless_unit #/ second)
    -- Simplification
    , checkParseUnit Succeed  "(m s/m)" (second)
    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ checkParseUnit Succeed  "m*kg" (meter #* kilo -| gram)
    , checkParseUnit Succeed  "km*g" (kilo -| meter #* gram)
    -- Commutative units
    , checkParseUnit Succeed  "(m s)" (meter #* second)
    , checkParseUnit Succeed  "(s m)" (meter #* second)
    , checkParseUnit Succeed  "(m s)" (second #* meter)
    -- Same dimension
    , checkParseUnit Succeed  "m*m"   (meter #* meter)
    , checkParseUnit Succeed  "km*m"  (kilo -| meter #* meter)
    -- Illegal wide notation
    , checkParseUnit Fail     "m s"   (meter #* second)
    ]
  , testGroup "Powers"
    [ checkParseUnit Succeed  "(m^2)"  (meter #^ 2)
    , checkParseUnit Succeed  "(km^2)" (kilo -| meter #^ 2)
    -- Simplification
    , checkParseUnit Succeed  "m^2/m"  (meter)
    ]
  ]
