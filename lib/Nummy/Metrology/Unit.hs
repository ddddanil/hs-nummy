module Nummy.Metrology.Unit (
  Unit, Quantity(..), Prefix, Modifier
, dimlessUnit, dimlessCoeff, conversion_ratio, complex_conversion, canonical_unit
, applyPrefix, applyModifier
, mkQu, quIn, dimOfUnit, dimOfQu
, (#^), (#!^), (#*), (#/)
, (%+), (%-), (%*), (%/), (%^), (%!^)
) where

import Protolude hiding (Prefix)
import Data.String (String)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Dimension


type Prefix = Value
type Modifier = Value
newtype Quantity = Quantity (Dimension, Value) deriving (Show, Eq) -- (Dimension, conversion to SI)
instance PP.Pretty Quantity where
  pretty (Quantity (d, v)) =
    PP.pretty (fromRational v :: Double)
    <> if not $ isDimless d
      then PP.char ' ' <> PP.pretty d
      else PP.empty

newtype Unit = Unit (Value -> Quantity)        -- s -> (a, s)  thats a State monad !!
instance PP.Pretty Unit where
  pretty (Unit u) = PP.pretty (u 1)

instance Eq Unit where
  u1 == u2 = 1 `mkQu` u1 == 1 `mkQu` u2

dimlessUnit :: Unit
dimlessUnit = Unit $ \v -> Quantity (dimless, v)

dimlessCoeff :: Value -> Unit
dimlessCoeff c = Unit $ \v -> Quantity (dimless, c * v)

canonical_unit :: Dimension -> Unit
canonical_unit dim = conversion_ratio dim 1

conversion_ratio :: Dimension -> Value -> Unit
conversion_ratio dim r = complex_conversion dim (*r)

complex_conversion :: Dimension -> (Value -> Value) -> Unit
complex_conversion dim f = Unit $ \v -> Quantity (dim, f v)

dimOfUnit :: Unit -> Dimension
dimOfUnit (Unit u) = dimOfQu $ u 1

dimOfQu :: Quantity -> Dimension
dimOfQu (Quantity (d, v)) = d

-- Unit and Quantity manipulation

applyPrefix :: Prefix -> Unit -> Unit
applyPrefix p (Unit u) = Unit $ \v -> (u v) %* (p `mkQu` dimlessUnit)

applyModifier :: Modifier -> Value -> Value
applyModifier m v = v * m

mkQu :: Value -> Unit -> Quantity
mkQu v (Unit u) = u v   -- haha

quIn :: Quantity -> Unit -> Maybe Quantity
quIn (Quantity (d, v)) (Unit u) =
  let Quantity (du, v') = u v
  in if d /= du then Nothing
  else Just $ Quantity (d, v')


-- Unit operators

infix 8 #!^
(#!^) :: Unit -> Unit -> Maybe Unit
u1 #!^ (Unit u2) =
  let Quantity (d, v) = u2 1
  in if isDimless d
     then Just (u1 #^ v)
     else Nothing

infixl 8 #^
(#^) :: Unit -> Value -> Unit
(Unit u) #^ p = Unit $
  \v ->
    let Quantity (d, v') = u 1
    in Quantity (d |^| p, v * (pow v' p))
  where
    pow :: Value -> Value -> Value
    pow v p =
      if denominator p == 1
      then v ^ (numerator p)
      else toRational $ fromRational v ** fromRational p

infixl 7 #*
(#*) :: Unit -> Unit -> Unit
(Unit u1) #* (Unit u2) = Unit $
  \v ->
    let Quantity (d1, v1) = u1 v
        Quantity (d2, v2) = u2 1
    in Quantity (d1 |*| d2, v1 * v2)

infixl 7 #/
(#/) :: Unit -> Unit -> Unit
(Unit u1) #/ (Unit u2) = Unit $
  \v ->
    let Quantity (d1, v1) = u1 v
        Quantity (d2, v2) = u2 1
    in Quantity (d1 |/| d2, v1 / v2)


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
