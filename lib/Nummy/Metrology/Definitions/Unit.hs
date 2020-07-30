module Nummy.Metrology.Definitions.Unit where

import Protolude ((*), (/), (+), (-))
import Nummy.Metrology.Definitions.Dimension
import Nummy.Metrology.Base
import Nummy.Metrology.Unit as U


-- = Base units
--
-- [source](https://en.wikipedia.org/wiki/SI_derived_unit)


dimless = U.dimless_unit

-- == Length units

meter = canonical_unit   length "m"

inch = conversion_ratio length "in" 0.0254

foot = conversion_ratio length "ft" (0.0254 * 12)              -- 1 ft = 12 in

yard = conversion_ratio length "yd" (0.0254 * 36)              -- 1 yd = 3 ft = 36 in

mile = conversion_ratio length "mi" (0.0254 * 12 * 5280)       -- 1 mi = 5280 ft


-- == Mass units

gram = conversion_ratio mass "g"   0.001

pound = conversion_ratio mass "lbs" 0.45359237

ounce = conversion_ratio mass "oz"  (0.45359237 / 16)


-- == Time units

second = canonical_unit   time "s"

minute = conversion_ratio time "min" 60

hour = conversion_ratio time "h"   3600


-- == Electrical current units

ampere = canonical_unit   current "A"


-- == Temperature units

kelvin = canonical_unit     temp "K"

celsius = complex_conversion temp "°C" (+273.15) (\t -> t - 273.15 :: Value)

fahrenheit = complex_conversion temp "°F" (\t -> 5 * (t + 459.67) / 9 :: Value)
                                          (\t -> 9 * t / 5 - 459.67 :: Value)

rankine = conversion_ratio   temp "R"  (5/9)


-- == Information units

bit = canonical_unit info "bit"

byte = conversion_ratio info "byte" 8


-- = Derived units

-- | Force
newton = canonical_unit force "N"

-- | Pressure
pascal = canonical_unit pressure "Pa"

-- | Energy
joule = canonical_unit energy "J"

-- | Power
watt = canonical_unit power "W"

-- | Frequency
hertz = canonical_unit frequency "Hz"

-- | Charge
coulomb = canonical_unit charge "C"

-- | Voltage
volt = canonical_unit voltage "V"

-- | Resistance
ohm = canonical_unit resistance "Ω"
