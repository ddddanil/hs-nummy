module Nummy.Metrology.Definitions.Tables where

import Protolude ((*), (/), (+), (-))
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
  -- Mass
  , (["g", "gram"],           gram          )
  , (["lbs", "pound"],        pound         )
  , (["oz", "ounce"],         ounce         )
  -- Time
  , (["s", "sec", "second"],  second        )
  , (["m", "min", "minute"],  minute        )
  , (["h", "hour"],           hour          )
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
  , (["N", "newton"],         newton        )
  , (["Pa", "pascal"],        pascal        )
  , (["J", "joule"],          joule         )
  , (["W", "watt"],           watt          )
  , (["Hz", "herz", "hertz"], hertz         )
  , (["C", "coulomb"],        coulomb       )
  , (["V", "volt"],           volt          )
  , (["O", "ohm"],            ohm           )
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
