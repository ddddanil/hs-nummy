module Main where

import Protolude
import Data.String (String)
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), Assertion, assertBool, assertEqual, assertFailure)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Nummy.Parser.Units
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
    [ testParse Succeed unit "m" $ unit' (Def.length, 1)
    , testParse Succeed unit "s" $ unit' (Def.time, 1)
    , testParse Succeed unit "g" $ unit' (Def.mass, 1%1000)
    , testParse Succeed unit "A" $ unit' (Def.current, 1)
    , testParse Succeed unit "K" $ unit' (Def.temp, 1)
    ]
  , testGroup "Syntax"
    [ testParse Fail unit ""               $ unit' (dimless, 1)
    -- optional parenthesis
    , testParse Succeed unit "(m)"         $ unit' (Def.length, 1)
    -- illegal spaces
    , testParse Fail unit "( m )"          $ unit' (Def.length, 1)
    , testParse Fail unit "(    m       )" $ unit' (Def.length, 1)
    , testParse Fail unit " m"             $ unit' (Def.length, 1)
    , testParse Fail unit "m "             $ unit' (Def.length, 1)
    , testParse Fail unit " m "            $ unit' (Def.length, 1)
    , testParse Fail unit "   m   "        $ unit' (Def.length, 1)
    , testParse Fail unit " (m)"           $ unit' (Def.length, 1)
    , testParse Fail unit "(m) "           $ unit' (Def.length, 1)
    , testParse Fail unit " (m) "          $ unit' (Def.length, 1)
    , testParse Fail unit "  (m)    "      $ unit' (Def.length, 1)
    ]
  , testGroup "Prefixes"
    [ testParse Succeed unit "mm" $ unit' (Def.length, 1%1000)
    , testParse Succeed unit "ms" $ unit' (Def.time, 1%1000)
    , testParse Succeed unit "kg" $ unit' (Def.mass, 1)
    ]
  , testGroup "Division"
    [ testParse Succeed unit "m/s"  $ unit' (Def.length |/| Def.time, 1)
    , testParse Succeed unit "km/h" $ unit' (Def.length |/| Def.time, 5 % 18)
    , testParse Succeed unit "1/s"  $ unit' (dimless |/| Def.time, 1)
    , testParse Succeed unit "2/km" $ unit' (dimless |/| Def.time, 2/1000)      -- Whats wrong with this thing
    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ testParse Succeed unit "m*kg" $ unit' (Def.length |*| Def.mass, 1)
    , testParse Succeed unit "km*g" $ unit' (Def.length |*| Def.mass, 1)
    -- Commutative units
    , testParse Succeed unit "(m s)" $ unit' (Def.length |*| Def.time, 1)
    , testParse Succeed unit "(s m)" $ unit' (Def.length |*| Def.time, 1)
    , testParse Succeed unit "(m s)" $ unit' (Def.time |*| Def.length, 1)
    -- Same dimension
    , testParse Succeed unit "m*m" $ unit' (Def.length |*| Def.length, 1)
    , testParse Succeed unit "km*m" $ unit' (Def.length |*| Def.length, 1000)
    ]
  , testGroup "Powers"
    [ testParse Succeed unit "(m^2)" $ unit' (Def.length |^| 2, 1)
    , testParse Succeed unit "(km^2)" $ unit' (Def.length |^| 2, 1000000)
    ]
  ]

testQuantities =
  localOption average_timeout $ testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ testParse Succeed quantity "10"    $ qu' (dimless, 10)
    , testParse Succeed quantity "7.5"   $ qu' (dimless, 15%2)
    , testParse Succeed quantity "999.2" $ qu' (dimless, 4996%5)
    -- Different allowed spacings
    , testParse Succeed quantity "5m"    $ qu' (Def.length, 5)
    , testParse Succeed quantity "6 m"   $ qu' (Def.length, 6)
    , testParse Succeed quantity "8(m)"  $ qu' (Def.length, 8)
    , testParse Succeed quantity "7 (m)" $ qu' (Def.length, 7)
    ]
  , testGroup "Modifiers"
    -- Dimensionless
    [ testParse Succeed quantity "6k"     $ qu' (dimless, 6000)
    -- Commutativity of modifiers and prefixes
    , testParse Succeed quantity "3k m"   $ qu' (Def.length, 3000)
    , testParse Succeed quantity "4 km"   $ qu' (Def.length, 4000)
    , testParse Succeed quantity "11k km" $ qu' (Def.length, 11000000)
    , testParse Succeed quantity "9k mm"  $ qu' (Def.length, 9)
    , testParse Succeed quantity "2m mg"  $ qu' (Def.mass, 2)
    ]
  , testGroup "Complex"
    [ testParse Succeed quantity "1.5m/s"      $ qu' (Def.length |/| Def.time, 3%2)
    , testParse Succeed quantity "9.8 (m/s s)" $ qu' (Def.length |/| (Def.time |*| Def.time), 49%5)
    ]
  ]

testDimensions =
  testGroup "Dimensions"
  [ testCase "len * len = len ^ 2" $ (Def.length |*| Def.length) @=? (Def.length |^| 2)
  ]

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testDimensions
  , testUnits
  , testQuantities
  ]


