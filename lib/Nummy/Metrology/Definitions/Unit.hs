{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_HADDOCK hide #-}
module Nummy.Metrology.Definitions.Unit where

import Nummy.Prelude ((*), (/), (+), (-))
import Nummy.Base
import Nummy.Metrology.Definitions.Dimension
import Nummy.Metrology.Unit as U


-- *** Base units
--
-- [source](https://en.wikipedia.org/wiki/SI_derived_unit)

-- | Unit of a scalar value
scalar_unit :: Unit
scalar_unit = U.scalar_unit

-- **** Length units

-- | Canonical (SI, MKS)
meter :: Unit
meter = canonical_unit   length "m"

-- | Base Imperial
--
-- 1 in = 25.4 mm
inch :: Unit
inch = conversion_ratio length "in" 0.0254

-- | Derived Imperial
--
-- 1 ft = 12 in
foot :: Unit
foot = conversion_ratio length "ft" (0.0254 * 12)

-- | Derived Imperial
--
-- 1 yd = 3 ft = 36 in
yard :: Unit
yard = conversion_ratio length "yd" (0.0254 * 36)

-- | Derived Imperial
--
-- 1 mi = 1760 yd = 5280 ft
mile :: Unit
mile = conversion_ratio length "mi" (0.0254 * 12 * 5280)

-- | Derived Imperial Maritime
--
-- 1 nm = 1852 m
naut_mile :: Unit
naut_mile = conversion_ratio length "n_mi" 1852


-- **** Mass units

-- | CGS
gram :: Unit
gram = conversion_ratio mass "g"   0.001

-- | Derived SI
--
-- 1 T = 1000 kg = 1 000 000 g
tonne :: Unit
tonne = conversion_ratio mass "T"  1000

-- | Canonical Imperial
--
-- 1 lbs = 0.453 592 37 kg
pound :: Unit
pound = conversion_ratio mass "lb" 0.453_592_37

-- | Derived Imperial
--
-- 1 oz = 1/16 lbs
ounce :: Unit
ounce = conversion_ratio mass "oz"  (0.453_592_37 / 16)

-- | Derived Imperial
--
-- 1 t = 2240 lbs
ton :: Unit
ton = conversion_ratio mass "t"  (0.453_592_37 * 2240)


-- **** Time units

-- | Canonical SI
second :: Unit
second = canonical_unit   time "s"

-- | Derived time
--
-- 1 min = 60 s
minute :: Unit
minute = conversion_ratio time "min" 60

-- | Derived time
--
-- 1 hour = 60 min = 3600 s
hour :: Unit
hour = conversion_ratio time "h"   3600

-- | Derived time
--
-- 1 day = 24 hour
day :: Unit
day = conversion_ratio time "day"  (3600 * 24)

-- | Derived time
--
-- 1 week = 7 day
week :: Unit
week = conversion_ratio time "week"  (3600 * 24 * 7)

-- | Derived time
--
-- 1 year = 365 day
year :: Unit
year = conversion_ratio time "year"  (3600 * 24 * 365)


-- **** Electrical current units

-- | Canonical SI
ampere :: Unit
ampere = canonical_unit   current "A"


-- **** Temperature units

-- | Canonical SI, absolute
kelvin :: Unit
kelvin = canonical_unit      temp "K"

-- | Derived SI, relative to 275.15 K
celsius :: Unit
celsius = complex_conversion temp "°C" (\t -> t + 273.15 :: Value)
                                       (\t -> t - 273.15 :: Value)

-- | Canonical Imperial
fahrenheit :: Unit
fahrenheit = complex_conversion temp "°F" (\t -> 5 * (t + 459.67) / 9 :: Value)
                                          (\t -> 9 * t / 5 - 459.67   :: Value)

-- | Derived Imperial, absolute   [wiki](https://en.wikipedia.org/wiki/Rankine_scale)
rankine :: Unit
rankine = conversion_ratio   temp "R"  (5/9)


-- **** Information units

-- | Atomic information
bit :: Unit
bit = canonical_unit info "bit"

-- | Conventional word
--
-- 1 byte = 8 bit
byte :: Unit
byte = conversion_ratio info "byte" 8


-- *** Derived units

-- **** Force

-- | Canonical (SI, MKS)
newton :: Unit
newton = canonical_unit force "N"

-- | CGS
dyne :: Unit
dyne = conversion_ratio force "dyn" (1/100_000)

-- | Imperial
pound_force :: Unit
pound_force = conversion_ratio force "lbf" 4.448_222

-- **** Pressure

-- | Canonical (SI, MKS)
pascal :: Unit
pascal = canonical_unit pressure "Pa"

-- | CGS
barye :: Unit
barye = conversion_ratio pressure "Ba" 0.1

-- **** Energy

-- | Canonical (SI, MKS)
joule :: Unit
joule = canonical_unit energy "J"

-- | CGS
erg :: Unit
erg = conversion_ratio energy "erg" (1/10_000_000)

-- **** Power

-- | Canonical (SI, MKS)
watt :: Unit
watt = canonical_unit power "W"

-- | Customary
horsepower :: Unit
horsepower = conversion_ratio power "hp" 735.49875

-- **** Frequency

-- | Canonical (SI, MKS)
hertz :: Unit
hertz = canonical_unit frequency "Hz"

-- **** Electrical charge

-- | Canonical (SI, MKS)
coulomb :: Unit
coulomb = canonical_unit charge "C"

-- **** Voltage

-- | Canonical (SI, MKS)
volt :: Unit
volt = canonical_unit voltage "V"

-- **** Electrical resistance

-- | Canonical (SI, MKS)
ohm :: Unit
ohm = canonical_unit resistance "Ω"

-- **** Electrical capacitance

-- | Canonical (SI, MKS)
farad :: Unit
farad = canonical_unit capacitance "F"
