{-# LANGUAGE TypeOperators #-}
module Nummy.Metrology.Quantity (
  Quantity
, dimOfQu
, (%#), (%<|)
, (%+), (%-), (%*), (%/), (%^)
) where

import Nummy.Prelude
import Data.Text.Prettyprint.Doc

import Nummy.Metrology.Base
import Nummy.Metrology.Dimension as D
import Nummy.Metrology.Unit as U


-- Type

-- | Quantity represents a value with a unit
newtype Quantity = Quantity (Value, Unit) deriving Show

instance Pretty Quantity where
  pretty (Quantity (v, u)) = pretty v <+> pretty u

instance Eq Quantity where
  Quantity (v1, u1) == Quantity (v2, u2) =
    (toSi u1) v1 == (toSi u2) v2 &&
    dimension u1 == dimension u2


-- | Access the dimension of a quantity
--
-- >>> dimOfQu (3 %# meter)
-- length
dimOfQu :: Quantity -> Dimension
dimOfQu (Quantity (_, u)) = dimension u


quIn :: Quantity -> Unit -> Maybe Quantity
quIn (Quantity (v, u)) u' =
  if dimension u /= dimension u' then Nothing
  else Just . Quantity $ (fromSi u' . toSi u $ v, u')

-- | Convert a quantity into a new unit
--
-- >>> 9 %# meter %<| foot
-- Just (29.52 ft)
-- >>> 1 %# meter %<| second
-- Nothing
infixl 3 %<|
(%<|) :: Quantity        -- ^ original quantity
      -> Unit            -- ^ Unit to cast into
      -> Maybe Quantity  -- ^ * Just qu -> successful conversion
                         --   * Nothing -> dimension of the quantity and unit were different
(%<|) = quIn


mkQu :: Value -> Unit -> Quantity
mkQu = curry Quantity

-- | Construct a quantity using a value and a unit
--
-- >>> 5 %# meter
-- 5 m
infixl 6 %#
(%#) :: Value
     -> Unit
     -> Quantity
(%#) = mkQu


-- Quantity operators

-- | Raise quantity to a power
--
-- >>> (2 %# meter) %^ 2
-- 4 m²
infixl 6 %^
(%^) :: Quantity -> Value -> Quantity
Quantity (v, u) %^ p = Quantity $ (v ^^^ p, u #^ p)

-- | Multiply quantities
--
-- >>> (2 %# meter) %* (3 %# second)
-- 6 m s
infixl 5 %*
(%*) :: Quantity -> Quantity -> Quantity
Quantity (v1, u1) %* Quantity (v2, u2) =
  Quantity $
    ( v1 * v2
    , u1 #* u2
    )

-- | Divide quantities
--
-- >>> (12 %# meter) %/ (3 %# second)
-- 4 m/s
infixl 5 %/
(%/) :: Quantity -> Quantity -> Quantity
Quantity (v1, u1) %/ Quantity (v2, u2) =
  Quantity $
    ( v1 / v2
    , u1 #/ u2
    )

-- | Add quantities
--
-- Dimensions of both quantities should be equal
--
-- >>> (3 %# second) %+ (1 %# second)
-- Just (4 s)
-- >>> (1 %# meter) %+ (1 %# second)
-- Nothing
infix 4 %+
(%+) :: Quantity -> Quantity -> Maybe Quantity
Quantity (v1, u1) %+ Quantity (v2, u2) =
  if dimension u1 /= dimension u2 then Nothing
  else Just . Quantity $ (fromSi u1 $ toSi u1 v1 + toSi u2 v2, u1)

-- | Subtract quantities
--
-- Dimensions of both quantities should be equal
--
-- >>> (6 %# meter) %- (2 %# meter)
-- Just (4 m)
-- >>> (1 %# meter) %- (1 %# gram)
-- Nothing
infix 4 %-
(%-) :: Quantity -> Quantity -> Maybe Quantity
Quantity (v1, u1) %- Quantity (v2, u2) =
  if dimension u1 /= dimension u2 then Nothing
  else Just . Quantity $ (fromSi u1 $ toSi u1 v1 - toSi u2 v2, u1)

