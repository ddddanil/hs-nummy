import Protolude
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertBool, assertEqual)
import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Nummy.Metrology.Definitions as Def (length, time, mass, current, temp)
import Nummy.Metrology.Dimension
import Nummy.Metrology.Show
import Nummy.Metrology.Unit
import Nummy.Parser.Units
import Nummy.Parser

short_timeout = Timeout 100 "0.1s"

testParse :: (Eq a, Show a) => Parser a -> String -> a -> TestTree
testParse p s ans = testCase s $ parse (parse_all p) "" s @?= Right ans

assertUnit :: (Dimension, Value) -> Unit -> Assertion
assertUnit x u = assertEqual "unit" x (u 1)

testUnit :: String -> (Dimension, Value) -> TestTree
testUnit s u =
  let Right ans = parse (parse_all unit) "" s
  in testCase s $ assertUnit u ans

testUnits =
  localOption short_timeout $ testGroup "Units"
  [ testGroup "Base"
    [ testUnit "m" (Def.length, 1)
    , testUnit "s" (Def.time, 1)
    , testUnit "g" (Def.mass, 1%1000)
    , testUnit "A" (Def.current, 1)
    , testUnit "K" (Def.temp, 1)
    -- optional parenthesis
    , testUnit "(m)" (Def.length, 1)
    , testUnit "( m )" (Def.length, 1)
    , testUnit "(    m       )" (Def.length, 1)
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
  localOption short_timeout $ testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ testParse quantity "10" (baseDim Dimensionless, 10)
    -- Different allowed spacings
    , testParse quantity "5m" (Def.length, 5)
    , testParse quantity "6 m" (Def.length, 6)
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
  ]

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testUnits
  , testQuantities
  ]


