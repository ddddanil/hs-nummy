{-# OPTIONS_HADDOCK hide #-}
module Nummy.Metrology.Definitions.Dimension where

import Nummy.Metrology.Dimension as D


-- *** Base dimensions

-- | Scalar   [wiki](https://en.wikipedia.org/wiki/Scalar_\(physics\))
scalar :: Dimension
scalar = D.scalar

-- | Length   [wiki](https://en.wikipedia.org/wiki/Length)
--
-- Canonical unit: 'Nummy.Metrology.Definitions.Unit.meter'
length :: Dimension
length = baseDim Length

-- | Mass   [wiki](https://en.wikipedia.org/wiki/Mass)
--
-- Canonical unit: 'kilo -| Nummy.Metrology.Definitions.Unit.gram'
mass :: Dimension
mass = baseDim Mass

-- | Time   [wiki](https://en.wikipedia.org/wiki/Time)
--
-- Canonical unit: 'Nummy.Metrology.Definitions.Unit.second'
time :: Dimension
time = baseDim Time

-- | Electric current   [wiki](https://en.wikipedia.org/wiki/Electric_current)
--
-- Canonical unit: 'Nummy.Metrology.Definitions.Unit.ampere'
current :: Dimension
current = baseDim Current

-- | Temperature   [wiki](https://en.wikipedia.org/wiki/Thermodynamic_temperature)
--
-- Canonical unit: 'Nummy.Metrology.Definitions.Unit.kelvin'
temp :: Dimension
temp = baseDim Temp

-- | Information   [wiki](https://en.wikipedia.org/wiki/Units_of_information)
--
-- Canonical unit: 'Nummy.Metrology.Definitions.Unit.bit'
info :: Dimension
info = baseDim Information

-- | Make currency distinct from scalar values
currency :: Dimension
currency = baseDim Currency


-- *** Derived dimensions

-- | Area   [wiki](https://en.wikipedia.org/wiki/Area)
area :: Dimension
area = length |^| 2

-- | Volume   [wiki](https://en.wikipedia.org/wiki/Volume)
volume :: Dimension
volume = length |^| 3

-- | Velocity   [wiki](https://en.wikipedia.org/wiki/Velocity)
velocity :: Dimension
velocity = length |/| time

-- | Acceleration   [wiki](https://en.wikipedia.org/wiki/Acceleration)
acceleration :: Dimension
acceleration = velocity |/| time

-- | [Impulse](https://en.wikipedia.org/wiki/Impulse_\(physics\)) \/ [momentum](https://en.wikipedia.org/wiki/Momentum)
impulse :: Dimension
impulse = mass |*| velocity

-- | Force   [wiki](https://en.wikipedia.org/wiki/Force)
force :: Dimension
force = mass |*| acceleration

-- | Pressure   [wiki](https://en.wikipedia.org/wiki/Pressure)
pressure :: Dimension
pressure = force |/| area

-- | Energy   [wiki](https://en.wikipedia.org/wiki/Energy)
energy :: Dimension
energy = force |*| length

-- | Power   [wiki](https://en.wikipedia.org/wiki/Power_\(physics\))
power :: Dimension
power = energy |/| time

-- | Frequency   [wiki](https://en.wikipedia.org/wiki/Frequency)
frequency :: Dimension
frequency = time |^| (-1)

-- | Charge   [wiki](https://en.wikipedia.org/wiki/Electric_charge)
charge :: Dimension
charge = current |*| time

-- | Voltage   [wiki](https://en.wikipedia.org/wiki/Voltage)
voltage :: Dimension
voltage = power |/| current

-- | Electrical resistance   [wiki](https://en.wikipedia.org/wiki/Electrical_resistance_and_conductance)
resistance :: Dimension
resistance = voltage |/| current

-- | Electrical capacitance   [wiki](https://en.wikipedia.org/wiki/Capacitance)
capacitance :: Dimension
capacitance = charge |/| voltage
