module Nummy.Metrology.Definitions (
  Value, Unit, Dimension
, (|*|), (|/|), (#*), (#/)
, baseUnitTable, prefixTable, modifierTable
, lookupUnit
, applyPrefix
-- , sanitizeDimension
-- , combineDimensions
) where

import Protolude
import Data.String (String)
import Data.List (lookup)
import GHC.Err (error)

-- General synonyms
type Value = Rational
type Label = String
type Dimension = [(Label, Value)]
type Unit = (Dimension, Value)  -- (Dimension, conversion to SI)


-- Table and its parts

baseDim :: Label -> Dimension
baseDim s = [(s, 1)]

symbol_table :: [ ([Label], Unit) ]  -- (Synonyms, )
symbol_table =
  -- Length
  [ (["m", "meter", "metre"], (baseDim "Length", 1))
  , (["ft", "foot", "feet"], (baseDim "Length", 0.3048))
  , (["mi", "mile"], (baseDim "Length", 1609.34))
  -- Mass
  , (["g", "gram", "gramm"], (baseDim "Mass", 1%1000))
  , (["lbs", "pound"], (baseDim "Mass", 0.453592))
  -- Time
  , (["s", "sec", "second"], (baseDim "Time", 1))
  , (["m", "min", "minute"], (baseDim "Time", 60))
  , (["h", "hour"], (baseDim "Time", 3600))
  -- Prefixes   km   ns
  , (["k", "kilo"], (baseDim "Prefix", 1000))
  , (["m", "milli"], (baseDim "Prefix", 1 % 1000))
  -- Number modifiers     1k  2m       Always one lowercase letter
  , (["k"], (baseDim "Modifier", 1000))
  , (["m"], (baseDim "Modifier", 1000000))
  ]

isPrefix :: Dimension -> Bool
isPrefix = (== [("Prefix", 1)])

isModifier :: Dimension -> Bool
isModifier = (== [("Modifier", 1)])

isBaseUnit :: Dimension -> Bool
isBaseUnit [(d, 1)] = d /= "Prefix" && d /= "Modifier"
isBaseUnit _ = False

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


-- Unit and Dimension manipulation

applyPrefix :: Unit -> Unit -> Unit
applyPrefix ([("Prefix", 1)], p) (d, value) = (d, p * value)
applyPrefix (_, _) _ = error "You must apply a prefix"

-- Remove prefixes, modifiers, with power 0 and merge same base dimensions
sanitizeDimension :: Dimension -> Dimension
sanitizeDimension = combineDimensions (+) [] . filter (\d -> not(prefix d || modifier d || noPower d) ) where
  prefix = (== "Prefix") . fst
  modifier = (== "Modifier") . fst
  noPower = (== 0) . snd

combineDimensions :: (Value -> Value -> Value) -> Dimension -> Dimension -> Dimension
combineDimensions op d1 d2 = unfoldr merge_same . sortOn fst $ d1 ++ d2  -- works bc sort is stable
  where
    merge_same ((d1,x):(d2,y):rest)
      | d1 == d2 = Just ((d1, op x y), rest)
      | otherwise = Just ((d1, x), (d2,y):rest)
    merge_same ((d,x):rest) = Just ((d,x), rest)
    merge_same _ = Nothing

infixl 7 |*|
(|*|) :: Dimension -> Dimension -> Dimension
(|*|) = (sanitizeDimension.) . combineDimensions (+)

infixl 7 |/|
(|/|) :: Dimension -> Dimension -> Dimension
d1 |/| d2 = sanitizeDimension . combineDimensions (+) d1 $ map (second negate) d2

infixl 7 #*
(#*) :: Unit -> Unit -> Unit
u1 #* u2 = ubimap (ubimap ((|*|), (*)) u1) u2 where
  ubimap = uncurry bimap

infixl 7 #/
(#/) :: Unit -> Unit -> Unit
u1 #/ u2 = ubimap (ubimap ((|/|), (/)) u1) u2 where
  ubimap = uncurry bimap
