module Main where

import Protolude
import Data.String (String)
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), Assertion, assertBool, assertEqual, assertFailure)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Nummy.Metrology.Definitions as Def (length, time, mass, current, temp)
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit

import Tests.Parser


short_timeout = Timeout 50 "50ms"
average_timeout = Timeout 333 "1/3s"
long_timeout = Timeout 1000 "1s"


testUnits =
  localOption average_timeout $ testGroup "Units"
  [ testGroup "Base"
    [ checkParseUnit Succeed "m" $ unit' (Def.length, 1)
    , checkParseUnit Succeed "s" $ unit' (Def.time, 1)
    , checkParseUnit Succeed "g" $ unit' (Def.mass, 1%1000)
    , checkParseUnit Succeed "A" $ unit' (Def.current, 1)
    , checkParseUnit Succeed "K" $ unit' (Def.temp, 1)
    ]
  , testGroup "Syntax"
    [ checkParseUnit Fail  ""               $ unit' (dimless, 1)
    -- optional parenthesis
    , checkParseUnit Succeed  "(m)"         $ unit' (Def.length, 1)
    -- illegal spaces
    , checkParseUnit Fail  "( m )"          $ unit' (Def.length, 1)
    , checkParseUnit Fail  "(    m       )" $ unit' (Def.length, 1)
    , checkParseUnit Fail  " m"             $ unit' (Def.length, 1)
    , checkParseUnit Fail  "m "             $ unit' (Def.length, 1)
    , checkParseUnit Fail  " m "            $ unit' (Def.length, 1)
    , checkParseUnit Fail  "   m   "        $ unit' (Def.length, 1)
    , checkParseUnit Fail  " (m)"           $ unit' (Def.length, 1)
    , checkParseUnit Fail  "(m) "           $ unit' (Def.length, 1)
    , checkParseUnit Fail  " (m) "          $ unit' (Def.length, 1)
    , checkParseUnit Fail  "  (m)    "      $ unit' (Def.length, 1)
    ]
  , testGroup "Prefixes"
    [ checkParseUnit Succeed  "mm" $ unit' (Def.length, 1%1000)
    , checkParseUnit Succeed  "ms" $ unit' (Def.time, 1%1000)
    , checkParseUnit Succeed  "kg" $ unit' (Def.mass, 1)
    ]
  , testGroup "Division"
    [ checkParseUnit Succeed  "m/s"  $ unit' (Def.length |/| Def.time, 1)
    , checkParseUnit Succeed  "km/h" $ unit' (Def.length |/| Def.time, 5 % 18)
    , checkParseUnit Succeed  "1/s"  $ unit' (dimless |/| Def.time, 1)
    , checkParseUnit Succeed  "2/km" $ unit' (dimless |/| Def.time, 2/1000)      -- Whats wrong with this thing
    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ checkParseUnit Succeed  "m*kg" $ unit' (Def.length |*| Def.mass, 1)
    , checkParseUnit Succeed  "km*g" $ unit' (Def.length |*| Def.mass, 1)
    -- Commutative units
    , checkParseUnit Succeed  "(m s)" $ unit' (Def.length |*| Def.time, 1)
    , checkParseUnit Succeed  "(s m)" $ unit' (Def.length |*| Def.time, 1)
    , checkParseUnit Succeed  "(m s)" $ unit' (Def.time |*| Def.length, 1)
    -- Same dimension
    , checkParseUnit Succeed  "m*m" $ unit' (Def.length |*| Def.length, 1)
    , checkParseUnit Succeed  "km*m" $ unit' (Def.length |*| Def.length, 1000)
    ]
  , testGroup "Powers"
    [ checkParseUnit Succeed  "(m^2)" $ unit' (Def.length |^| 2, 1)
    , checkParseUnit Succeed  "(km^2)" $ unit' (Def.length |^| 2, 1000000)
    ]
  ]

testQuantities =
  localOption average_timeout $ testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ checkParseQu Succeed "10"    $ qu' (dimless, 10)
    , checkParseQu Succeed "7.5"   $ qu' (dimless, 15%2)
    , checkParseQu Succeed "999.2" $ qu' (dimless, 4996%5)
    -- Different allowed spacings
    , checkParseQu Succeed "5m"    $ qu' (Def.length, 5)
    , checkParseQu Succeed "6 m"   $ qu' (Def.length, 6)
    , checkParseQu Succeed "8(m)"  $ qu' (Def.length, 8)
    , checkParseQu Succeed "7 (m)" $ qu' (Def.length, 7)
    ]
  , testGroup "Modifiers"
    -- Dimensionless
    [ checkParseQu Succeed "6k"     $ qu' (dimless, 6000)
    -- Commutativity of modifiers and prefixes
    , checkParseQu Succeed "3k m"   $ qu' (Def.length, 3000)
    , checkParseQu Succeed "4 km"   $ qu' (Def.length, 4000)
    , checkParseQu Succeed "11k km" $ qu' (Def.length, 11000000)
    , checkParseQu Succeed "9k mm"  $ qu' (Def.length, 9)
    , checkParseQu Succeed "2m mg"  $ qu' (Def.mass, 2)
    ]
  , testGroup "Complex"
    [ checkParseQu Succeed "1.5m/s"      $ qu' (Def.length |/| Def.time, 3%2)
    , checkParseQu Succeed "9.8 (m/s s)" $ qu' (Def.length |/| (Def.time |*| Def.time), 49%5)
    ]
  ]

testDimensions =
  testGroup "Dimensions"
  [ checkDim Succeed "len * len = len ^ 2" (Def.length |*| Def.length) (Def.length |^| 2)
  ]

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testDimensions
  , testUnits
  , testQuantities
  ]


