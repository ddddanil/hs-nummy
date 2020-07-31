module Tests.Dimensions (testDimensions) where

import Nummy.Prelude hiding (length, force)

import Test.Tasty  (TestTree, testGroup, localOption)

import Nummy.Metrology (Quantity(..), Unit, Dimension, Prefix
                       , (|^|), (|*|), (|/|)
                       , (-|), (#^), (#*), (#/)
                       , (%#), (%<|), (%^), (%*), (%/), (%+), (%-)
                       )
import Nummy.Metrology.Definitions
import Nummy.Metrology.Definitions.Dimension
import Tests.Definitions
import Tests.Parser (checkDim)


testDimensions =
  -- localOption average_timeout $
  testGroup "Dimensions"
  [ testGroup "Algebra"
    [ checkDim Succeed "len * len == len ^ 2" (length |*| length) (length |^| 2)
    , checkDim Succeed "len * time / time == len" (length |*| time |/| time) (length)
    , checkDim Succeed "time * len / time == len" (time |*| length |/| time) (length)
    , checkDim Succeed "len ^2 / len == len" (length |^| 2 |/| length) (length)
    ]
  , testGroup "Alternative definitions"
    -- Area
    [ checkDim Succeed "area = length ^ 2" (area) (length |^| 2)
    , checkDim Succeed "area = length * length" (area) (length |*| length)
    -- Volume
    , checkDim Succeed "vol = length ^ 3" (volume) (length |^| 3)
    , checkDim Succeed "vol = length ^ 2 * length" (volume) (length |^| 2 |*| length)
    , checkDim Succeed "vol = length * length ^ 2" (volume) (length |*| length |^| 2)
    , checkDim Succeed "vol = length * length * length" (volume) (length |*| length |*| length)
    -- Acceleration
    , checkDim Succeed "acc = velocity / time" (acceleration) (velocity |/| time)
    , checkDim Succeed "acc = length / time ^ 2" (acceleration) (length |/| time |^| 2)
    -- Force
    , checkDim Succeed "force = mass * acc" (force) (mass |*| acceleration)
    , checkDim Succeed "force = mass * velocity / time" (force) (mass |*| velocity |/| time)
    , checkDim Succeed "force = mass * len / time ^ 2" (force) (mass |*| length |/| time |^| 2)
    -- Energy
    , checkDim Succeed "energy = mass * length ^ 2 / time ^ 2" (energy) (mass |*| (length |^| 2) |/| (time |^| 2))
    , checkDim Succeed "energy = length * force" (energy) (length |*| force)
    , checkDim Succeed "energy = charge * voltage" (energy) (charge |*| voltage)
    , checkDim Succeed "energy = power * time" (energy) (power |*| time)
    -- Power
    , checkDim Succeed "power = mass * length ^ 2 / time ^ 3" (power) (mass |*| length |^| 2 |/| time |^| 3)
    , checkDim Succeed "power = energy / time" (power) (energy |/| time)
    , checkDim Succeed "power = voltage * current" (power) (voltage |*| current)
    -- Charge
    , checkDim Succeed "charge = time * current" (charge) (time |*| current)
    -- , checkDim Succeed "charge = capacitance * voltage" (charge) (capacitance |*| voltage)
    ]
  ]
