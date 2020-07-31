module Nummy.Metrology.Dimension (
  Dimension, BaseDim(..)
, baseDim, dimless
, (|*|), (|/|), (|^|)
) where

import Nummy.Prelude hiding (Prefix)
import qualified Data.Map.Strict as M
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Base


-- | Base dimensions
--
-- [wiki](https://en.wikipedia.org/wiki/International_System_of_Units#Base_units)
data BaseDim = Length | Mass | Time | Current | Temp | Information deriving (Eq, Ord, Show)

instance PP.Pretty BaseDim where
  pretty Length      = PP.text "m"
  pretty Mass        = PP.text "kg"
  pretty Time        = PP.text "s"
  pretty Current     = PP.text "A"
  pretty Temp        = PP.text "K"
  pretty Information = PP.text "bit"


-- | A dimension represents a product of several base dimensions raised to some power
--
-- [wiki](https://en.wikipedia.org/wiki/International_System_of_Units#Derived_units)
newtype Dimension = Dimension { factors :: M.Map BaseDim Value } deriving (Show)

-- instance PP.Pretty Dimension where

instance Eq Dimension where
  d1 == d2 = f d1 == f d2 where f = factors . sanitizeDimension


-- Working with dims

baseDim :: BaseDim -> Dimension
baseDim b = Dimension $ M.singleton b 1

-- | Dimension of a scalar value
dimless :: Dimension
dimless = Dimension $ M.empty

-- | Remove components with exponent 0
sanitizeDimension :: Dimension -> Dimension
sanitizeDimension (Dimension dim) = Dimension $ M.filter (/= 0) dim


-- Operators

-- | Raise dimension to a power
infixl 8 |^|
(|^|) :: Dimension -> Value -> Dimension
(Dimension d1) |^| v = Dimension $ M.map (*v) d1

-- | Product of two dimensions
infixl 7 |*|
(|*|) :: Dimension -> Dimension -> Dimension
(Dimension d1) |*| (Dimension d2) = sanitizeDimension . Dimension $ M.unionWith (+) d1 d2

-- | Quotient of two dimensions
infixl 7 |/|
(|/|) :: Dimension -> Dimension -> Dimension
d1 |/| d2 = d1 |*| d2 |^| (-1)

