{-# OPTIONS_HADDOCK hide #-}
module Nummy.Metrology.Definitions.Tables where

import Nummy.Prelude ((*), (/), (+), (-))
import Nummy.Metrology.Base
import Nummy.Metrology.Unit
import Nummy.Metrology.Definitions.Unit
import Nummy.Metrology.Definitions.Prefix


unit_table :: [ ([Label], Unit) ]  -- ^ (Synonyms, Unit)
unit_table =
  -- Length
  [ (["m", "meter", "metre"], meter         )
  , (["in", "inch"],          inch          )
  , (["ft", "foot", "feet"],  foot          )
  , (["yd", "yard"],          yard          )
  , (["mi", "mile"],          mile          )
  , (["n_mi", "naut_mile"],   naut_mile     )
  -- Mass
  , (["g", "gram"],           gram          )
  , (["T", "tonne"],          tonne         )
  , (["lb", "pound"],         pound         )
  , (["oz", "ounce"],         ounce         )
  , (["t", "ton"],            ton           )
  -- Time
  , (["s", "sec", "second"],  second        )
  , (["m", "min", "minute"],  minute        )
  , (["h", "hour"],           hour          )
  , (["day"],                 day           )
  , (["week"],                week          )
  , (["year"],                year          )
  -- Current
  , (["A", "amp", "ampere"],  ampere        )
  -- Temp
  , (["K", "kelvin"],         kelvin        )
  , (["C", "celsius"],        celsius       )
  , (["F", "fahrenheit"],     fahrenheit    )
  , (["R", "rankine"],        rankine       )
  -- Temp differences
  , (["dK", "dC"],            kelvin        )
  , (["dF", "dR"],            rankine       )
  -- Information
  , (["bit"],                 bit           )
  , (["byte"],                byte          )

  -- Derived
  -- Force
  , (["N", "newton"],         newton        )
  , (["dyn", "dyne"],         dyne          )
  , (["lbf"],                 pound_force   )
  -- Pressure
  , (["Pa", "pascal"],        pascal        )
  , (["Ba", "barye"],         barye         )
  -- Energy
  , (["J", "joule"],          joule         )
  , (["erg"],                 erg           )
  -- Power
  , (["W", "watt"],           watt          )
  -- Frequency
  , (["Hz", "herz", "hertz"], hertz         )
  -- Charge
  , (["C", "coulomb"],        coulomb       )
  -- Voltage
  , (["V", "volt"],           volt          )
  -- Resistance
  , (["O", "ohm"],            ohm           )
  -- Capacitance
  , (["F", "farad"],          farad         )
  ]

-- | [Source](https://en.wikipedia.org/wiki/Metric_prefix)
prefix_table :: [ ([Label], Prefix) ]  -- ^ (Synonyms, Prefix)
prefix_table =
  [ (["Y", "yotta"],  yotta  )
  , (["Z", "zetta"],  zetta  )
  , (["E", "exa"],    exa    )
  , (["P", "peta"],   peta   )
  , (["T", "tera"],   tera   )
  , (["G", "giga"],   giga   )
  , (["M", "mega"],   mega   )
  , (["k", "kilo"],   kilo   )
  , (["h", "hecto"],  hecto  )
  , (["da", "deca"],  deca   )
  , (["d", "deci"],   deci   )
  , (["c", "centi"],  centi  )
  , (["m", "milli"],  milli  )
  , (["mu", "micro"], micro  )
  , (["n", "nano"],   nano   )
  , (["p", "pico"],   pico   )
  , (["f", "femto"],  femto  )
  , (["a", "atto"],   atto   )
  , (["z", "zepto"],  zepto  )
  , (["y", "yocto"],  yocto  )
  ]
