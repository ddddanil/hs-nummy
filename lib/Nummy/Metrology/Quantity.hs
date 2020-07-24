module Nummy.Metrology.Quantity (
  Quantity(..)
, dimOfQu
, (%+), (%-), (%*), (%/), (%^), (%!^)
) where

import Protolude
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Dimension


-- Type

newtype Quantity = Quantity (Dimension, Value) deriving (Show, Eq) -- (Dimension, conversion to SI)

instance PP.Pretty Quantity where
  pretty (Quantity (d, v)) =
    PP.pretty (fromRational v :: Double)
    <> if not $ isDimless d
      then PP.char ' ' <> PP.pretty d
      else PP.empty


dimOfQu :: Quantity -> Dimension
dimOfQu (Quantity (d, v)) = d


-- Quantity operators

infixl 8 %!^
(%!^) :: Quantity -> Quantity -> Maybe Quantity
q %!^ (Quantity (d, v)) =
  if isDimless d
  then Just (q %^ v)
  else Nothing

infixl 8 %^
(%^) :: Quantity -> Value -> Quantity
(Quantity (d1, v1)) %^ p = Quantity $ (d1 |^| p, pow v1 p)
  where
    pow :: Value -> Value -> Value
    pow v p =
      if denominator p == 1
      then v ^ (numerator p)
      else toRational $ fromRational v ** fromRational p

infixl 7 %*
(%*) :: Quantity -> Quantity -> Quantity
(Quantity u1) %* (Quantity u2) = Quantity $ ubimap (ubimap ((|*|), (*)) u1) u2 where
  ubimap = uncurry bimap

infixl 7 %/
(%/) :: Quantity -> Quantity -> Quantity
(Quantity u1) %/ (Quantity u2) = Quantity $ ubimap (ubimap ((|/|), (/)) u1) u2 where
  ubimap = uncurry bimap

infix 6 %+
(%+) :: Quantity -> Quantity -> Maybe Quantity
(Quantity (d1, v1)) %+ (Quantity (d2, v2)) = if d1 /= d2 then Nothing
                                             else Just $ Quantity (d1, v1 + v2)

infix 6 %-
(%-) :: Quantity -> Quantity -> Maybe Quantity
(Quantity (d1, v1)) %- (Quantity (d2, v2)) = if d1 /= d2 then Nothing
                                             else Just $ Quantity (d1, v1 - v2)
