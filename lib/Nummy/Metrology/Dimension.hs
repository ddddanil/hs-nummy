module Nummy.Metrology.Dimension (
  Value, Label, Dimension
, (|*|), (|/|)
) where

import Protolude
import Data.String (String)

type Value = Rational
type Label = String
type Dimension = [(Label, Value)]


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


-- Operators

infixl 7 |*|
(|*|) :: Dimension -> Dimension -> Dimension
(|*|) = (sanitizeDimension.) . combineDimensions (+)

infixl 7 |/|
(|/|) :: Dimension -> Dimension -> Dimension
d1 |/| d2 = sanitizeDimension . combineDimensions (+) d1 $ map (second negate) d2


