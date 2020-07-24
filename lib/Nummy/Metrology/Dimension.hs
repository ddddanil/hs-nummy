module Nummy.Metrology.Dimension (
  Value, Label, Dimension(..), BaseDim(..)
, baseDim, isBaseUnit, isDimless, isNone
, (|*|), (|/|), (|^|)
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.List (lookup, foldl1)


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
sanitizeDimension (Dimension dim) = combineDimensions (+) (Dimension []) . Dimension . filter (\d -> power d && has_dim d ) $ dim
  where power = (/= 0) . snd; has_dim = (/= Dimensionless) . fst

combineDimensions :: (Value -> Value -> Value) -> Dimension -> Dimension -> Dimension
combineDimensions op (Dimension d1) (Dimension d2) = Dimension $ sort . map (second $ foldl1 op) . groupAssoc $ d1 ++ d2

groupAssoc :: (Eq a) => [(a, b)] -> [(a, [b])]
groupAssoc = foldl add []
  where
    add :: (Eq a) => [(a, [b])] -> (a, b) -> [(a, [b])]
    add acc (k, v) =
      case lookup k acc of
        Just _ -> map (\(k', vs) -> if k == k' then (k, vs ++ [v]) else (k', vs) ) acc
        Nothing -> (k,[v]):acc

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
