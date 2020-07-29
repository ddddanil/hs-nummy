module Nummy.Metrology.Dimension (
  Dimension, BaseDim(..)
, baseDim, dimless
, isBaseUnit, isDimless
, (|*|), (|/|), (|^|)
, Nummy.Metrology.Dimension.length, mass, time, current, temp
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.List (lookup, partition, foldl1)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Base

-- Types

data BaseDim = Length | Mass | Time | Current | Temp deriving (Eq, Ord, Show)
instance PP.Pretty BaseDim where
  pretty Length  = PP.text "m"
  pretty Mass    = PP.text "kg"
  pretty Time    = PP.text "s"
  pretty Current = PP.text "A"
  pretty Temp    = PP.text "K"

newtype Dimension = Dimension { factors :: [(BaseDim, Value)] } deriving (Show)

instance PP.Pretty Dimension where
  pretty (Dimension dim) =
    if null dim
    then PP.empty
    else
    if show num_line == ("" :: Text)
    then PP.char '1'
    else num_line
    <>
    if show den_line == ("" :: Text)
    then PP.empty
    else PP.char '/' <> den_line
    where
      (num, den) = partition ((> 0) . snd) dim
      print_term (d, p) = PP.pretty d <> if abs p /= 1 then PP.char '^' <> PP.pretty p else PP.empty
      num_line = PP.hsep $ map print_term num
      den_line = PP.hsep $ map print_term den

instance Eq Dimension where
  d1 == d2 = f d1 == f d2 where f = factors . sanitizeDimension

-- Working with dims

baseDim :: BaseDim -> Dimension
baseDim b = Dimension [(b, 1)]

dimless :: Dimension
dimless = Dimension []

isDimless :: Dimension -> Bool
isDimless (Dimension d ) = d == []

isBaseUnit :: Dimension -> Bool
isBaseUnit (Dimension [(d, 1)]) = True
isBaseUnit _ = False

-- Remove components with power 0 and merge same base dimensions
sanitizeDimension :: Dimension -> Dimension
sanitizeDimension (Dimension dim) = combineDimensions (+) (Dimension []) . Dimension . filter (\d -> power d ) $ dim
  where power = (/= 0) . snd

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


-- Base dim definitions

length = baseDim Length
mass = baseDim Mass
time = baseDim Time
current = baseDim Current
temp = baseDim Temp
