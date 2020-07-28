module Nummy.Metrology.Quantity (
  Quantity(..)
, dimOfQu
, mkQu, quIn
, (%+), (%-), (%*), (%/), (%^), (%!^)
) where

import Protolude
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Dimension as D
import Nummy.Metrology.Unit as U

-- Type

newtype Quantity = Quantity (Value, Unit)

instance PP.Pretty Quantity where
  pretty (Quantity (v, u)) =
    PP.pretty (fromRational v :: Double)
    <> if not $ U.isDimless u
      then PP.char ' ' <> PP.pretty (dimOfUnit u)
      else PP.empty

instance Eq Quantity where
  Quantity (v1, u1) == Quantity (v2, u2) =
    (convert u1 u2) v1 == v2


dimOfQu :: Quantity -> Dimension
dimOfQu (Quantity (d, u)) = dimOfUnit u

quIn :: Quantity -> Unit -> Maybe Quantity
quIn (Quantity (v, u)) u' =
  if dimOfUnit u /= dimOfUnit u' then Nothing
  else Just . Quantity $ ((convert u u') v, u')

mkQu :: Value -> Unit -> Quantity
mkQu = curry Quantity

-- Quantity operators

infixl 8 %!^
(%!^) :: Quantity -> Quantity -> Maybe Quantity
q %!^ Quantity (v, u) =
  if U.isDimless u
  then Just (q %^ v)
  else Nothing

infixl 8 %^
(%^) :: Quantity -> Value -> Quantity
Quantity (v, u) %^ p = Quantity $ (pow v p, u #^ p)
  where
    pow :: Value -> Value -> Value
    pow v p =
      if denominator p == 1
      then v ^ (numerator p)
      else toRational $ fromRational v ** fromRational p

infixl 7 %*
(%*) :: Quantity -> Quantity -> Quantity
Quantity (v1, u1) %* Quantity (v2, u2) =
  Quantity $
    ( v1 * v2
    , u1 #* u2
    )

infixl 7 %/
(%/) :: Quantity -> Quantity -> Quantity
Quantity (v1, u1) %/ Quantity (v2, u2) =
  Quantity $
    ( v1 / v2
    , u1 #/ u2
    )

infix 6 %+
(%+) :: Quantity -> Quantity -> Maybe Quantity
Quantity (v1, u1) %+ Quantity (v2, u2) =
  if dimOfUnit u1 /= dimOfUnit u2 then Nothing
  else Just . Quantity $ (v1 + (convert u2 u1) v2, u1)

infix 6 %-
(%-) :: Quantity -> Quantity -> Maybe Quantity
Quantity (v1, u1) %- Quantity (v2, u2) =
  if dimOfUnit u1 /= dimOfUnit u2 then Nothing
  else Just . Quantity $ (v1 - (convert u2 u1) v2, u1)

