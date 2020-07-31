module Nummy.Metrology.Dimension (
  Dimension, BaseDim(..)
, baseDim, dimless
, (|*|), (|/|), (|^|)
) where

import Nummy.Prelude hiding (Prefix)
import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc

import Nummy.Metrology.Base


-- | Base dimensions
--
-- [wiki](https://en.wikipedia.org/wiki/International_System_of_Units#Base_units)
data BaseDim = Length | Mass | Time | Current | Temp | Information | Currency deriving (Eq, Ord, Show)

instance Pretty BaseDim where
  pretty Length      = pretty ("length" :: Text)
  pretty Mass        = pretty ("mass" :: Text)
  pretty Time        = pretty ("time" :: Text)
  pretty Current     = pretty ("current" :: Text)
  pretty Temp        = pretty ("temperature" :: Text)
  pretty Information = pretty ("information" :: Text)
  pretty Currency    = pretty ("currency" :: Text)


-- | A dimension represents a product of several base dimensions raised to some power
--
-- [wiki](https://en.wikipedia.org/wiki/International_System_of_Units#Derived_units)
newtype Dimension = Dimension { factors :: M.Map BaseDim Value } deriving (Show)

instance Pretty Dimension where
  pretty (Dimension d) = top <> bottom where
    (num, den) = M.partition (>0) d
    pretty_base b p = pretty b <> pretty_power (abs p)
    pretty_power 1 = mempty
    pretty_power 2 = pretty '²'
    pretty_power 3 = pretty '³'
    pretty_power x = pretty '^' <> pretty x
    top = if null num then pretty '1' else hcat $ M.elems . M.mapWithKey pretty_base $ num
    bottom = if null den then mempty else hcat $ (pretty '/' :) . M.elems . M.mapWithKey pretty_base $ den

instance Eq Dimension where
  d1 == d2 =
    factors d1 == factors d2


-- Working with dims

baseDim :: BaseDim -> Dimension
baseDim b = Dimension $ M.singleton b 1

-- | Dimension of a scalar value
dimless :: Dimension
dimless = Dimension $ M.empty


-- Operators

-- | Raise dimension to a power
infixl 8 |^|
(|^|) :: Dimension -> Value -> Dimension
(Dimension d1) |^| v = Dimension $ M.map (*v) d1

-- | Product of two dimensions
infixl 7 |*|
(|*|) :: Dimension -> Dimension -> Dimension
(Dimension d1) |*| (Dimension d2) =  Dimension $ M.filter (/= 0) $ M.unionWith (+) d1 d2

-- | Quotient of two dimensions
infixl 7 |/|
(|/|) :: Dimension -> Dimension -> Dimension
d1 |/| d2 = d1 |*| d2 |^| (-1)

