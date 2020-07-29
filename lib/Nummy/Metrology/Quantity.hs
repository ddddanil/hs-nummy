{-# LANGUAGE TypeOperators #-}
module Nummy.Metrology.Quantity (
  Quantity(..)
, dimOfQu
, mkQu, quIn
, (%+), (%-), (%*), (%/), (%^)
) where

import Protolude
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Base
import Nummy.Metrology.Dimension as D
import Nummy.Metrology.Unit as U

-- Type

newtype Quantity = Quantity (Value, Unit)

instance PP.Pretty Quantity where
  pretty (Quantity (v, u)) = PP.pretty v PP.<+> PP.pretty u

instance Eq Quantity where
  Quantity (v1, u1) == Quantity (v2, u2) =
    (toSi u1) v1 == (toSi u2) v2


dimOfQu :: Quantity -> Dimension
dimOfQu (Quantity (d, u)) = dimension u

quIn :: Quantity -> Unit -> Maybe Quantity
quIn (Quantity (v, u)) u' =
  if dimension u /= dimension u' then Nothing
  else Just . Quantity $ (fromSi u' . toSi u $ v, u')

mkQu :: Value -> Unit -> Quantity
mkQu = curry Quantity


-- Quantity operators

infixl 8 %^
(%^) :: Quantity -> Value -> Quantity
Quantity (v, u) %^ p = Quantity $ (v ^^^ p, u #^ p)

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
  if dimension u1 /= dimension u2 then Nothing
  else Just . Quantity $ (fromSi u1 $ toSi u1 v1 + toSi u2 v2, u1)

infix 6 %-
(%-) :: Quantity -> Quantity -> Maybe Quantity
Quantity (v1, u1) %- Quantity (v2, u2) =
  if dimension u1 /= dimension u2 then Nothing
  else Just . Quantity $ (fromSi u1 $ toSi u1 v1 - toSi u2 v2, u1)

