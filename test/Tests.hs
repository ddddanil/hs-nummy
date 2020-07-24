import Protolude
import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Nummy.Metrology.Definitions as Def (length, time, mass)
import Nummy.Metrology.Dimension
import Nummy.Metrology.Show
import Nummy.Metrology.Unit
import Nummy.Parser.Units

parseTest :: (Eq a, Show a) => Parser a -> String -> a -> TestTree
parseTest p s ans = testCase s $ parse p "" s @?= Right ans

testUnits =
  testGroup "Units"
  [ testGroup "Base"
    [ parseTest unit "m" (Def.length, 1)
    , parseTest unit "s" (Def.time, 1)
    , parseTest unit "g" (Def.mass, 1%1000)
    ]
  , testGroup "Prefixes"
    [ parseTest unit "mm" (Def.length, 1%1000)
    , parseTest unit "ms" (Def.time, 1%1000)
    , parseTest unit "kg" (Def.mass, 1)
    ]
  , testGroup "Division"
    [ parseTest unit "m/s" (Def.length |/| Def.time, 1)
    , parseTest unit "km/h" (Def.length |/| Def.time, 5 % 18)
    ]
  , testGroup "Multiplication"
    -- Commutative prefixes
    [ parseTest unit "m*kg" (Def.length |*| Def.mass, 1)
    , parseTest unit "km*g" (Def.length |*| Def.mass, 1)
    -- Commutative units
    , parseTest unit "(m s)" (Def.length |*| Def.time, 1)
    , parseTest unit "(s m)" (Def.length |*| Def.time, 1)
    , parseTest unit "(m s)" (Def.time |*| Def.length, 1)
    -- Same dimension
    , parseTest unit "m*m" (Def.length |*| Def.length, 1)
    , parseTest unit "km*m" (Def.length |*| Def.length, 1000)
    ]
  ]

testQuantities =
  testGroup "Quantities"
  [ testGroup "Base"
    -- Dimensionless
    [ parseTest quantity "10" (baseDim Dimensionless, 10)
    -- Different allowed spacings
    , parseTest quantity "5m" (Def.length, 5)
    , parseTest quantity "6 m" (Def.length, 6)
    , parseTest quantity "7 (m)" (Def.length, 7)
    ]
  , testGroup "Modifiers"
    -- Dimensionless
    [ parseTest quantity "6k" (baseDim Dimensionless, 6000)
    , parseTest quantity "12m" (baseDim Dimensionless, 12000000)
    -- Commutativity of modifiers and prefixes
    , parseTest quantity "3k m" (Def.length, 3000)
    , parseTest quantity "4 km" (Def.length, 4000)
    , parseTest quantity "11k km" (Def.length, 11000000)
    , parseTest quantity "9k mm" (Def.length, 9)
    , parseTest quantity "2m mg" (Def.mass, 2)
    ]
  ]

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testUnits
  , testQuantities
  ]


