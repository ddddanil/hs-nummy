module Nummy.Metrology.Definitions (
  baseUnitTable, prefixTable, modifierTable
, lookupUnit, lookupPrefix, lookupModifier
) where

import Protolude hiding (length, Prefix)
import Data.String (String)
import Data.List (lookup)

import Nummy.Metrology.Base
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit


-- Table and its parts

base_unit_table :: [ ([Label], Unit) ]  -- (Synonyms, Unit)
base_unit_table =
  -- Length
  [ (["m", "meter", "metre"], canonical_unit   length "m"                            )
  , (["in", "inch"],          conversion_ratio length "in" 0.00254                   )
  , (["ft", "foot", "feet"],  conversion_ratio length "ft" (0.00254 * 12)            ) -- 1 ft = 12 in
  , (["yd", "yard"],          conversion_ratio length "yd" (0.00254 * 36)            ) -- 1 yd = 3 ft = 36 in
  , (["mi", "mile"],          conversion_ratio length "mi" (0.00254 * 12 * 5280)     ) -- 1 mi = 5280 ft
  -- Mass
  , (["g", "gram"],           conversion_ratio mass "g"   0.001                      )
  , (["lbs", "pound"],        conversion_ratio mass "lbs" 0.45359237                 )
  , (["oz", "ounce"],         conversion_ratio mass "oz"  (0.45359237 / 16)          )
  -- Time
  , (["s", "sec", "second"],  canonical_unit   time "s"                              )
  , (["m", "min", "minute"],  conversion_ratio time "min" 60                         )
  , (["h", "hour"],           conversion_ratio time "h"   3600                       )
  -- Current
  , (["A", "Amp", "amp"],     canonical_unit   current "A"                           )
  -- Temp
  , (["K", "Kelvin"],         canonical_unit     temp "K"                            )
  , (["C", "Celsius"],        complex_conversion temp "°C" (+273.15) (\t -> t - 273.15 :: Value))
  , (["F", "Fahrenheit"],     complex_conversion temp "°F" (\t -> 5 * (t + 459.67) / 9 :: Value)
                                                           (\t -> 9 * t / 5 - 459.67 :: Value)  )
  , (["R", "Rankine"],        conversion_ratio   temp "R"  (5/9)                     )
  -- Temp differences
  , (["dK", "dC"],            canonical_unit     temp "K"                            )
  , (["dF", "dR"],            conversion_ratio   temp "R"  (5/9)                     )
  ]

prefix_table :: [ ([Label], Prefix) ]
prefix_table =
  [ (["k", "kilo"],  (1000, "k")    )
  , (["m", "milli"], (1/1000, "m"))
  ]

modifier_table :: [ ([Label], Modifier)]
modifier_table =
  -- Always one lowercase letter
  [ (["k"], 1000    )
  , (["m"], 1000000 )
  ]

baseUnitTable :: [Label]
baseUnitTable = concat . map fst $ base_unit_table

prefixTable :: [Label]
prefixTable = concat . map fst $ prefix_table

modifierTable :: [Label]
modifierTable = concat . map fst $ modifier_table


-- Lookups

lookupUnit :: Maybe Dimension -> Label -> Maybe Unit
lookupUnit md unit =
  let has_unit = filter matches_unit base_unit_table
  in case md of
    Just dim -> snd <$> find (matches_dimension dim) has_unit
    Nothing -> snd <$> headMay has_unit
  where
    matches_unit = elem unit . fst
    matches_dimension dim = (==dim) . dimension . snd

lookupPrefix :: Label -> Maybe Prefix
lookupPrefix p = snd <$> find (elem p . fst) prefix_table

lookupModifier :: Label -> Maybe Modifier
lookupModifier m = snd <$> find (elem m . fst) modifier_table

