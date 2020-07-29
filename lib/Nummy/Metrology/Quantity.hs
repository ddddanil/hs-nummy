{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DatatypeContexts #-}
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

newtype (Unit a) => Quantity a = Quantity (Value, a)

instance (PP.Pretty a, Unit a) => PP.Pretty (Quantity a) where
  pretty (Quantity (v, u)) = PP.pretty v PP.<+> PP.pretty u

instance (Unit a) => Eq (Quantity a) where
  Quantity (v1, u1) == Quantity (v2, u2) =
    (toSi u1) v1 == (toSi u2) v2


dimOfQu :: (Unit a) => Quantity a -> Dimension
dimOfQu (Quantity (d, u)) = dimension u

quIn :: (Unit a) => Quantity a -> a -> Maybe (Quantity a)
quIn (Quantity (v, u)) u' =
  if dimension u /= dimension u' then Nothing
  else Just . Quantity $ (fromSi u' . toSi u $ v, u')

mkQu :: (Unit a) => Value -> a -> Quantity a
mkQu = curry Quantity


-- Quantity operators

infixl 8 %^
(%^) :: (Unit a) => Quantity a -> Value -> Quantity (a #^ Value)
Quantity (v, u) %^ p = Quantity $ (v ^^^ p, u #^ p)

infixl 7 %*
(%*) :: (Unit a, Unit b) => Quantity a -> Quantity b -> Quantity (a #* b)
Quantity (v1, u1) %* Quantity (v2, u2) =
  Quantity $
    ( v1 * v2
    , u1 #* u2
    )

infixl 7 %/
(%/) :: (Unit a, Unit b) => Quantity a -> Quantity b -> Quantity (a #/ b)
Quantity (v1, u1) %/ Quantity (v2, u2) =
  Quantity $
    ( v1 / v2
    , u1 #/ u2
    )

infix 6 %+
(%+) :: (Unit a, Unit b) => Quantity a -> Quantity b -> Maybe (Quantity a)
Quantity (v1, u1) %+ Quantity (v2, u2) =
  if dimension u1 /= dimension u2 then Nothing
  else Just . Quantity $ (fromSi u1 $ toSi u1 v1 + toSi u2 v2, u1)

infix 6 %-
(%-) :: (Unit a, Unit b) => Quantity a -> Quantity b -> Maybe (Quantity a)
Quantity (v1, u1) %- Quantity (v2, u2) =
  if dimension u1 /= dimension u2 then Nothing
  else Just . Quantity $ (fromSi u1 $ toSi u1 v1 - toSi u2 v2, u1)

