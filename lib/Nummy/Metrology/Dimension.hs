module Nummy.Metrology.Dimension (
  Value, Label, Dimension(..), BaseDim(..)
, baseDim, isBaseUnit, isDimless, isNone
, (|*|), (|/|), (|^|)
) where

import Protolude hiding (Prefix)
import Data.String (String)


-- Types

type Value = Rational
type Label = String
newtype Dimension = Dimension [(BaseDim, Value)] deriving Eq

data BaseDim = Dimensionless | Length | Mass | Time | Current | Temp deriving (Eq, Ord)


-- Working with dims

baseDim :: BaseDim -> Dimension
baseDim b = Dimension [(b, 1)]

isNone :: Dimension -> Bool
isNone (Dimension d ) = d == []

isDimless :: Dimension -> Bool
isDimless (Dimension d ) = d == [(Dimensionless, 1)]

isBaseUnit :: Dimension -> Bool
isBaseUnit (Dimension [(d, 1)]) = True
isBaseUnit _ = False

-- Remove components with power 0 and merge same base dimensions
sanitizeDimension :: Dimension -> Dimension
sanitizeDimension (Dimension dim) = combineDimensions (+) (Dimension []) . Dimension . filter (\d -> not $ noPower d ) $ dim
  where noPower = (== 0) . snd

combineDimensions :: (Value -> Value -> Value) -> Dimension -> Dimension -> Dimension
combineDimensions op (Dimension d1) (Dimension d2) = Dimension $ unfoldr merge_same . sortOn fst $ d1 ++ d2  -- works bc sort is stable
  where
    merge_same ((d1,x):(d2,y):rest)
      | d1 == d2 = Just ((d1, op x y), rest)
      | otherwise = Just ((d1, x), (d2,y):rest)
    merge_same ((d,x):rest) = Just ((d,x), rest)
    merge_same _ = Nothing


-- Operators

infixl 8 |^|
(|^|) :: Dimension -> Value -> Dimension
(Dimension d1) |^| v = Dimension $ map (second (*v)) d1

infixl 7 |*|
(|*|) :: Dimension -> Dimension -> Dimension
(|*|) = (sanitizeDimension.) . combineDimensions (+)

infixl 7 |/|
(|/|) :: Dimension -> Dimension -> Dimension
d1 |/| (Dimension d2) = sanitizeDimension . combineDimensions (+) d1 $ Dimension $ map (second negate) d2
