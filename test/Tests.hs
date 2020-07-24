import Protolude ((%))
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), Assertion, assertBool, assertEqual, assertFailure)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Nummy.Metrology.Definitions as Def (length, time, mass, current, temp)
import Nummy.Metrology.Dimension
import Nummy.Metrology.Show
import Nummy.Metrology.Unit
import Nummy.Parser.Units
import Nummy.Parser

short_timeout = Timeout 50 "50ms"
average_timeout = Timeout 150 "150ms"

testParse :: (Eq a, Show a) => Parser a -> String -> a -> TestTree
testParse p s ans = testCase (show s) $ parse (parse_all p) "" s @?= Right ans

assertUnit :: (Dimension, Value) -> Unit -> Assertion
assertUnit x u = assertEqual "unit" x (u 1)

testUnit :: String -> (Dimension, Value) -> TestTree
testUnit s u = testCase (show s) $ assertion
  where
    assertion =
      case parse (parse_all unit) "" s of
        Right ans -> assertUnit u ans
        Left err -> assertFailure (show err)


testUnits =
  localOption average_timeout $ testGroup "Units"
  [ testGroup "Base"
    [ testUnit "m" (Def.length, 1)
    , testUnit "s" (Def.time, 1)
    , testUnit "g" (Def.mass, 1%1000)
    , testUnit "A" (Def.current, 1)
    , testUnit "K" (Def.temp, 1)
    ]
  , testGroup "Syntax"
    [ expectFail $ testUnit "" (baseDim Dimensionless, 1)
    -- optional parenthesis
    , testUnit "(m)" (Def.length, 1)
    -- illegal spaces
    , expectFail $ testUnit "( m )" (Def.length, 1)
    , expectFail $ testUnit "(    m       )" (Def.length, 1)
    , expectFail $ testUnit " m" (Def.length, 1)
    , expectFail $ testUnit "m " (Def.length, 1)
    , expectFail $ testUnit " m " (Def.length, 1)
    , expectFail $ testUnit "   m   " (Def.length, 1)
    , expectFail $ testUnit " (m)" (Def.length, 1)
    , expectFail $ testUnit "(m) " (Def.length, 1)
    , expectFail $ testUnit " (m) " (Def.length, 1)
    , expectFail $ testUnit "  (m)    " (Def.length, 1)
    ]
  , testGroup "Prefixes"
    [ testUnit "mm" (Def.length, 1%1000)
    , testUnit "ms" (Def.time, 1%1000)
    , testUnit "kg" (Def.mass, 1)
    ]
  , testGroup "Division"
    [ testUnit "m/s" (Def.length |/| Def.time, 1)
    , testUnit "km/h" (Def.length |/| Def.time, 5 % 18)
    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ testUnit "m*kg" (Def.length |*| Def.mass, 1)
    , testUnit "km*g" (Def.length |*| Def.mass, 1)
    -- Commutative units
    , testUnit "(m s)" (Def.length |*| Def.time, 1)
    , testUnit "(s m)" (Def.length |*| Def.time, 1)
    , testUnit "(m s)" (Def.time |*| Def.length, 1)
    -- Same dimension
    , testUnit "m*m" (Def.length |*| Def.length, 1)
    , testUnit "km*m" (Def.length |*| Def.length, 1000)
    ]
  ]

testQuantities =
  localOption average_timeout $ testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ testParse quantity "10" (baseDim Dimensionless, 10)
    , testParse quantity "7.5" (baseDim Dimensionless, 15%2)
    , testParse quantity "999.2" (baseDim Dimensionless, 4996%5)
    -- Different allowed spacings
    , testParse quantity "5m" (Def.length, 5)
    , testParse quantity "6 m" (Def.length, 6)
    , testParse quantity "8(m)" (Def.length, 8)
    , testParse quantity "7 (m)" (Def.length, 7)
    ]
  , testGroup "Modifiers"
    -- Dimensionless
    [ testParse quantity "6k" (baseDim Dimensionless, 6000)
    -- Commutativity of modifiers and prefixes
    , testParse quantity "3k m" (Def.length, 3000)
    , testParse quantity "4 km" (Def.length, 4000)
    , testParse quantity "11k km" (Def.length, 11000000)
    , testParse quantity "9k mm" (Def.length, 9)
    , testParse quantity "2m mg" (Def.mass, 2)
    ]
  , testGroup "Complex"
    [ testParse quantity "1.5m/s" (Def.length |/| Def.time, 3%2)
    , testParse quantity "9.8 (m/s s)" (Def.length |/| (Def.time |*| Def.time), 49%5)
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


