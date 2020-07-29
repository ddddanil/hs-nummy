module Nummy.Metrology.Definitions (
  baseUnitTable, prefixTable, modifierTable
, lookupUnit, lookupPrefix, lookupModifier
) where

import Protolude hiding (length, Prefix)
import Data.String (String)
import Data.List (lookup)

import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit


-- Table and its parts

symbol_table :: [ ([Label], Unit) ]  -- (Synonyms, )
symbol_table =
  -- Length
  [ (["m", "meter", "metre"], canonical_unit   "m"  length                           )
  , (["in", "inch"],          conversion_ratio "in" length 0.00254                   )
  , (["ft", "foot", "feet"],  conversion_ratio "ft" length (0.00254 * 12)            ) -- 1 ft = 12 in
  , (["yd", "yard"],          conversion_ratio "yd" length (0.00254 * 36)            ) -- 1 yd = 3 ft = 36 in
  , (["mi", "mile"],          conversion_ratio "mi" length (0.00254 * 12 * 5280)     ) -- 1 mi = 5280 ft
  -- Mass
  , (["g", "gram"],           conversion_ratio "g"   mass 0.001                       )
  , (["lbs", "pound"],        conversion_ratio "lbs" mass 0.45359237                  )
  , (["oz", "ounce"],         conversion_ratio "oz"  mass (0.45359237 / 16)           )
  -- Time
  , (["s", "sec", "second"],  canonical_unit   "s"   time                             )
  , (["m", "min", "minute"],  conversion_ratio "min" time 60                          )
  , (["h", "hour"],           conversion_ratio "h"   time 3600                        )
  -- Current
  , (["A", "Amp", "amp"],     canonical_unit   "A"   current                            )
  -- Temp
  , (["K", "Kelvin"],         canonical_unit     "K"  temp                           )
  , (["C", "Celsius"],        complex_conversion "°C" temp (+273.15) (\t -> t - 273.15))
  , (["F", "Fahrenheit"],     complex_conversion "°F" temp (\t -> 5%9 * (t + 459.67))
                                                      (\t -> 9%5 * t - 459.67)  )
  , (["R", "Rankine"],        conversion_ratio   "R"  temp (5%9)                     )
  -- Temp differences
  , (["dK", "dC"],            canonical_unit     "K" temp                           )
  , (["dF", "dR"],            conversion_ratio   "R" temp (5%9)                     )
  ]

prefix_table :: [ ([Label], Prefix) ]
prefix_table =
  [ (["k", "kilo"],  (1000, "k")    )
  , (["m", "milli"], (1 % 1000, "m"))
  ]

modifier_table :: [ ([Label], Modifier)]
modifier_table =
  -- Always one lowercase letter
  [ (["k"], 1000    )
  , (["m"], 1000000 )
  ]

unitTable :: [Label]
unitTable = concat . map fst $ symbol_table

baseUnitTable :: [Label]
baseUnitTable = concat . map fst $ symbol_table

prefixTable :: [Label]
prefixTable = concat . map fst $ prefix_table

modifierTable :: [Label]
modifierTable = concat . map fst $ modifier_table


-- Lookups

lookupUnit :: Maybe Dimension -> Label -> Maybe Unit
lookupUnit md unit =
  let has_unit = filter matches_unit symbol_table
  in case md of
    Just dim -> snd <$> find (matches_dimension dim) has_unit
    Nothing -> snd <$> headMay has_unit
  where
    matches_unit = elem unit . fst
    matches_dimension dim = (==dim) . dimOfUnit . snd

lookupPrefix :: Label -> Maybe Prefix
lookupPrefix p = snd <$> find (elem p . fst) prefix_table

lookupModifier :: Label -> Maybe Modifier
lookupModifier m = snd <$> find (elem m . fst) modifier_table

