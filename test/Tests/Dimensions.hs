module Tests.Dimensions (testDimensions) where

import Nummy.Prelude (($))

import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology ((|^|), (|*|), (|/|))
import Nummy.Metrology.Definitions
import Tests.Definitions
import Tests.Parser (checkDim)


testDimensions :: TestTree
testDimensions =
  localOption average_timeout $
  testGroup "Dimensions"
  [ testGroup "Algebra"
    [ checkDim Equal "len * len == len ^ 2" (length |*| length) (length |^| 2)
    , checkDim Equal "len * time / time == len" (length |*| time |/| time) (length)
    , checkDim Equal "time * len / time == len" (time |*| length |/| time) (length)
    , checkDim Equal "len ^2 / len == len" (length |^| 2 |/| length) (length)
    ]
  , testGroup "Alternative definitions"
    -- Area
    [ checkDim Equal "area = length ^ 2" (area) (length |^| 2)
    , checkDim Equal "area = length * length" (area) (length |*| length)
    -- Volume
    , checkDim Equal "vol = length ^ 3" (volume) (length |^| 3)
    , checkDim Equal "vol = length ^ 2 * length" (volume) (length |^| 2 |*| length)
    , checkDim Equal "vol = length * length ^ 2" (volume) (length |*| length |^| 2)
    , checkDim Equal "vol = length * length * length" (volume) (length |*| length |*| length)
    -- Acceleration
    , checkDim Equal "acc = velocity / time" (acceleration) (velocity |/| time)
    , checkDim Equal "acc = length / time ^ 2" (acceleration) (length |/| time |^| 2)
    -- Force
    , checkDim Equal "force = mass * acc" (force) (mass |*| acceleration)
    , checkDim Equal "force = mass * velocity / time" (force) (mass |*| velocity |/| time)
    , checkDim Equal "force = mass * len / time ^ 2" (force) (mass |*| length |/| time |^| 2)
    -- Energy
    , checkDim Equal "energy = mass * length ^ 2 / time ^ 2" (energy) (mass |*| (length |^| 2) |/| (time |^| 2))
    , checkDim Equal "energy = length * force" (energy) (length |*| force)
    , checkDim Equal "energy = charge * voltage" (energy) (charge |*| voltage)
    , checkDim Equal "energy = power * time" (energy) (power |*| time)
    -- Power
    , checkDim Equal "power = mass * length ^ 2 / time ^ 3" (power) (mass |*| length |^| 2 |/| time |^| 3)
    , checkDim Equal "power = energy / time" (power) (energy |/| time)
    , checkDim Equal "power = voltage * current" (power) (voltage |*| current)
    -- Charge
    , checkDim Equal "charge = time * current" (charge) (time |*| current)
    , checkDim Equal "charge = capacitance * voltage" (charge) (capacitance |*| voltage)
    ]
  ]
