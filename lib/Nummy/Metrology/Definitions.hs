module Nummy.Metrology.Definitions (
  baseUnitTable, prefixTable, modifierTable
, lookupUnit
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
  [ (["m", "meter", "metre"], (length, 1))
  , (["ft", "foot", "feet"], (length, 0.3048))
  , (["mi", "mile"], (length, 1609.34))
  -- Mass
  , (["g", "gram", "gramm"], (mass, 1%1000))
  , (["lbs", "pound"], (mass, 0.453592))
  -- Time
  , (["s", "sec", "second"], (time, 1))
  , (["m", "min", "minute"], (time, 60))
  , (["h", "hour"], (time, 3600))
  -- Prefixes   km   ns
  , (["k", "kilo"], (baseDim Prefix, 1000))
  , (["m", "milli"], (baseDim Prefix, 1 % 1000))
  -- Number modifiers     1k  2m       Always one lowercase letter
  , (["k"], (baseDim Modifier, 1000))
  , (["m"], (baseDim Modifier, 1000000))
  ]

baseUnitTable :: [Label]
baseUnitTable = concat . map fst . filter not_base $ symbol_table where
  not_base = isBaseUnit . fst . snd

prefixTable :: [Label]
prefixTable = concat . map fst . filter is_prefix $ symbol_table where
  is_prefix = isPrefix . fst . snd

modifierTable :: [Label]
modifierTable = concat . map fst . filter is_prefix $ symbol_table where
  is_prefix = isModifier . fst . snd

lookupUnit :: Maybe Dimension -> Label -> Maybe Unit
lookupUnit md unit = case md of
  Just dim -> snd <$> (find (matches_dimension dim) . filter matches_unit) symbol_table
  Nothing -> snd <$> find matches_unit symbol_table
  where
    matches_unit = elem unit . fst
    matches_dimension dim = (==dim) . fst . snd


-- Base dim definitions

length = baseDim Length
mass = baseDim Mass
time = baseDim Time
