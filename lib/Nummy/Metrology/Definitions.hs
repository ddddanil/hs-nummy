{-# LANGUAGE TemplateHaskellQuotes #-}

module Nummy.Metrology.Definitions (
  Value, Unit
, unitTable, prefixTable, modifierTable
, lookupUnit
, applyPrefix
) where

import Protolude
import Data.String (String)
import GHC.Err (error)

-- General synonyms
type Value = Rational
type Label = String
type Unit = (Label, Value)  -- (Dimension, conversion to SI)
type ComplexUnit = Ratio Unit

symbol_table :: [ ([Label], Unit) ]  -- (Synonyms, )
symbol_table =
  -- Length
  [ (["m", "meter", "metre"], ("Length", 1))
  , (["ft", "foot", "feet"], ("Length", 0.3048))
  , (["mi", "mile"], ("Length", 1609.34))
  -- Mass
  , (["g", "gram", "gramm"], ("Mass", 1%1000))
  , (["lbs", "pound"], ("Mass", 0.453592))
  -- Prefixes   km   ns
  , (["k", "kilo"], ("Prefix", 1000))
  , (["m", "milli"], ("Prefix", 1 % 1000))
  -- Number modifiers     1k  2m       Always one lowercase letter
  , (["k"], ("Modifier", 1000))
  , (["m"], ("Modifier", 1000000))
  ]

unitTable :: [Label]
unitTable = concat . map fst . filter is_unit $ symbol_table where
  is_unit = (\d -> d /= "Prefix" && d /= "Modifier") . fst . snd

prefixTable :: [Label]
prefixTable = concat . map fst . filter is_prefix $ symbol_table where
  is_prefix = (== "Prefix") . fst . snd

modifierTable :: [Label]
modifierTable = concat . map fst . filter is_prefix $ symbol_table where
  is_prefix = (== "Prefix") . fst . snd

lookupUnit :: Maybe Label -> Label -> Maybe Unit
lookupUnit md unit = case md of
  Just dim -> snd <$> (find (matches_dimension dim) . filter matches_unit) symbol_table
  Nothing -> snd <$> find matches_unit symbol_table
  where
    matches_unit = elem unit . fst
    matches_dimension dim = (==dim) . fst . snd

applyPrefix :: Unit -> Unit -> Unit
applyPrefix ("Prefix", p) (d, value) = (d, p * value)
applyPrefix (_, _) _ = error "You must apply a prefix"
