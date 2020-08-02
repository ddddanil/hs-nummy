module Nummy.Metrology.Definitions.Tables (
  unit_table
, prefix_table
) where

import Nummy.Base
import Nummy.Metrology.Unit
import Nummy.Metrology.Prefix
import Nummy.Metrology.Definitions.Unit
import Nummy.Metrology.Definitions.Prefix


no_prefix     :: [PrefixType]
no_prefix     = [ ]
metric_prefix :: [PrefixType]
metric_prefix = [ PrefixAboveOne, PrefixBelowOne ]
info_prefix   :: [PrefixType]
info_prefix   = [ PrefixAboveOne, PrefixBinary ]


unit_table :: [ ([Label], Unit, [PrefixType]) ]  -- ^ (Synonyms, Unit)
unit_table =
  -- Length
  [ (["m", "meter", "metre"], meter       , metric_prefix  )
  , (["in", "inch"],          inch        , no_prefix      )
  , (["ft", "foot", "feet"],  foot        , no_prefix      )
  , (["yd", "yard"],          yard        , no_prefix      )
  , (["mi", "mile"],          mile        , no_prefix      )
  , (["n_mi", "naut_mile"],   naut_mile   , no_prefix      )
  -- Mass
  , (["g", "gram"],           gram        , metric_prefix  )
  , (["T", "tonne"],          tonne       , metric_prefix  )
  , (["lb", "pound"],         pound       , no_prefix      )
  , (["oz", "ounce"],         ounce       , no_prefix      )
  , (["t", "ton"],            ton         , no_prefix      )
  -- Time
  , (["s", "sec", "second"],  second      , metric_prefix  )
  , (["m", "min", "minute"],  minute      , no_prefix      )
  , (["h", "hour"],           hour        , no_prefix      )
  , (["day"],                 day         , no_prefix      )
  , (["week"],                week        , no_prefix      )
  , (["year"],                year        , no_prefix      )
  -- Current
  , (["A", "amp", "ampere"],  ampere      , metric_prefix  )
  -- Temp
  , (["K", "kelvin"],         kelvin      , metric_prefix  )
  , (["C", "celsius"],        celsius     , no_prefix      )
  , (["F", "fahrenheit"],     fahrenheit  , no_prefix      )
  , (["R", "rankine"],        rankine     , no_prefix      )
  -- Temp differences
  , (["dK", "dC"],            kelvin      , no_prefix      )
  , (["dF", "dR"],            rankine     , no_prefix      )
  -- Information
  , (["b", "bit"],            bit         , info_prefix    )
  , (["B", "byte"],           byte        , info_prefix    )

  -- Derived
  -- Force
  , (["N", "newton"],         newton      , metric_prefix  )
  , (["dyn", "dyne"],         dyne        , metric_prefix  )
  , (["lbf"],                 pound_force , no_prefix      )
  -- Pressure
  , (["Pa", "pascal"],        pascal      , metric_prefix  )
  , (["Ba", "barye"],         barye       , metric_prefix  )
  -- Energy
  , (["J", "joule"],          joule       , metric_prefix  )
  , (["erg"],                 erg         , metric_prefix  )
  , (["Wh"],                  watt #* hour, metric_prefix  )
  , (["cal"],                 calorie     , metric_prefix  )
  , (["Cal", "calorie"],      kilo -| calorie, no_prefix   )
  , (["eV", "electron-volt"], electron_volt, metric_prefix )
  -- Power
  , (["W", "watt"],           watt        , metric_prefix  )
  , (["hp", "horsepower"],    horsepower  , no_prefix      )
  -- Frequency
  , (["Hz", "herz", "hertz"], hertz       , metric_prefix  )
  -- Charge
  , (["C", "coulomb"],        coulomb     , metric_prefix  )
  -- Voltage
  , (["V", "volt"],           volt        , metric_prefix  )
  -- Resistance
  , (["O", "ohm"],            ohm         , metric_prefix  )
  -- Capacitance
  , (["F", "farad"],          farad       , metric_prefix  )
  ]


-- | [Source](https://en.wikipedia.org/wiki/Metric_prefix)
prefix_table :: [ ([Label], Prefix, PrefixType) ]  -- ^ (Synonyms, Prefix)
prefix_table =
  -- Above one
  [ (["Y", "yotta"],  yotta , PrefixAboveOne )
  , (["Z", "zetta"],  zetta , PrefixAboveOne )
  , (["E", "exa"],    exa   , PrefixAboveOne )
  , (["P", "peta"],   peta  , PrefixAboveOne )
  , (["T", "tera"],   tera  , PrefixAboveOne )
  , (["G", "giga"],   giga  , PrefixAboveOne )
  , (["M", "mega"],   mega  , PrefixAboveOne )
  , (["k", "kilo"],   kilo  , PrefixAboveOne )
  , (["h", "hecto"],  hecto , PrefixAboveOne )
  , (["da", "deca"],  deca  , PrefixAboveOne )
  -- Below one
  , (["d", "deci"],   deci  , PrefixBelowOne )
  , (["c", "centi"],  centi , PrefixBelowOne )
  , (["m", "milli"],  milli , PrefixBelowOne )
  , (["mu", "micro"], micro , PrefixBelowOne )
  , (["n", "nano"],   nano  , PrefixBelowOne )
  , (["p", "pico"],   pico  , PrefixBelowOne )
  , (["f", "femto"],  femto , PrefixBelowOne )
  , (["a", "atto"],   atto  , PrefixBelowOne )
  , (["z", "zepto"],  zepto , PrefixBelowOne )
  , (["y", "yocto"],  yocto , PrefixBelowOne )
  -- Binary
  , (["Ki", "kibi"],  kibi  , PrefixBinary   )
  , (["Mi", "mebi"],  mebi  , PrefixBinary   )
  , (["Gi", "gibi"],  gibi  , PrefixBinary   )
  , (["Ti", "tebi"],  tebi  , PrefixBinary   )
  , (["Pi", "pebi"],  pebi  , PrefixBinary   )
  , (["Ei", "exbi"],  exbi  , PrefixBinary   )
  , (["Zi", "zebi"],  zebi  , PrefixBinary   )
  , (["Yi", "yobi"],  yobi  , PrefixBinary   )
  ]
