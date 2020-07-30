module Nummy.Metrology.Definitions (
  unitTable, prefixTable
, lookupUnit, lookupPrefix
) where

import Protolude hiding (length, Prefix)
import Data.String (String)
import Data.List (lookup)

import Nummy.Metrology.Base
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit


-- Table and its parts

unit_table :: [ ([Label], Unit) ]  -- (Synonyms, Unit)
unit_table =
  -- Length
  [ (["m", "meter", "metre"], canonical_unit   length "m"                            )
  , (["in", "inch"],          conversion_ratio length "in" 0.0254                   )
  , (["ft", "foot", "feet"],  conversion_ratio length "ft" (0.0254 * 12)            ) -- 1 ft = 12 in
  , (["yd", "yard"],          conversion_ratio length "yd" (0.0254 * 36)            ) -- 1 yd = 3 ft = 36 in
  , (["mi", "mile"],          conversion_ratio length "mi" (0.0254 * 12 * 5280)     ) -- 1 mi = 5280 ft
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
  [ (["k", "kilo"],  Prefix (1000, "k")    )
  , (["m", "milli"], Prefix (1/1000, "m"))
  ]

-- | All unit synonyms
unitTable :: [Label]
unitTable = concat . map fst $ unit_table

-- | All prefix synonyms
prefixTable :: [Label]
prefixTable = concat . map fst $ prefix_table


-- Lookups

-- | Find a unit
--
-- Specifying a dimension will narrow down the search
--
-- >>> lookupUnit Nothing "m"
-- Just meter
-- >>> lookupUnit (Just time) "m"
-- Just minute
-- >>> lookupUnit Nothing "x"
-- Nothing
lookupUnit :: Maybe Dimension -- ^ Optional dimension specifier
           -> Label           -- ^ Unit synonym
           -> Maybe Unit      -- ^ Result
lookupUnit md unit =
  let has_unit = filter matches_unit unit_table
  in case md of
    Just dim -> snd <$> find (matches_dimension dim) has_unit
    Nothing -> snd <$> headMay has_unit
  where
    matches_unit = elem unit . fst
    matches_dimension dim = (==dim) . dimension . snd

-- | Find a prefix
--
-- >>> lookupPrefix "k"
-- Just kilo
-- >>> lookupPrefix "x"
-- Nothing
lookupPrefix :: Label -> Maybe Prefix
lookupPrefix p = snd <$> find (elem p . fst) prefix_table
