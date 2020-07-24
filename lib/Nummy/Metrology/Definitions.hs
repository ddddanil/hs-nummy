module Nummy.Metrology.Definitions (
  baseUnitTable, prefixTable, modifierTable
, lookupUnit, lookupPrefix, lookupModifier
, length, mass, time, current, temp
) where

import Protolude hiding (length, Prefix)
import Data.String (String)
import Data.List (lookup)

import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit


-- Table and its parts

symbol_table :: [ ([Label], Unit) ]  -- (Synonyms, )
symbol_table =
  -- -- Dimless tokens
  -- [ (["1"],                   dimlessUnit                                       )
  -- Length
  [ (["m", "meter", "metre"], canonical_unit   length                           )
  , (["ft", "foot", "feet"],  conversion_ratio length 0.3048                    )
  , (["mi", "mile"],          conversion_ratio length 1609.34                   )
  -- Mass
  , (["g", "gram"],           conversion_ratio mass 0.001                       )
  , (["lbs", "pound"],        conversion_ratio mass 0.453592                    )
  -- Time
  , (["s", "sec", "second"],  canonical_unit   time                             )
  , (["m", "min", "minute"],  conversion_ratio time 60                          )
  , (["h", "hour"],           conversion_ratio time 3600                        )
  -- Current
  , (["A", "Amp", "amp"],     canonical_unit current                            )
  -- Temp
  , (["K", "Kelvin"],         canonical_unit     temp                           )
  , (["C", "Celsius"],        complex_conversion temp (+273.15)                 )
  , (["F", "Fahrenheit"],     complex_conversion temp (\t -> 5%9 * (t + 459.67)))
  ]

prefix_table :: [ ([Label], Prefix) ]
prefix_table =
  [ (["k", "kilo"],  1000    )
  , (["m", "milli"], 1 % 1000)
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


-- Base dim definitions

length = baseDim Length
mass = baseDim Mass
time = baseDim Time
current = baseDim Current
temp = baseDim Temp
