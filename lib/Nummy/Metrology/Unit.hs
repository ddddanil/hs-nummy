module Nummy.Metrology.Unit (
  Unit, Quantity, Prefix, Modifier
, dimlessUnit, conversion_ratio, complex_conversion, canonical_unit
, applyPrefix, applyModifier
, mkQu, quIn, ofDim
, (#*), (#/)
, (%+), (%-), (%*), (%/)
) where

import Protolude hiding (Prefix)
import Data.String (String)
import GHC.Err (error)

import Nummy.Metrology.Dimension


type Prefix = Value
type Modifier = Value
type Quantity = (Dimension, Value) -- (Dimension, conversion to SI)
type Unit = Value -> Quantity      -- s -> (a, s)  thats a State monad !!


dimlessUnit :: Unit
dimlessUnit = \v -> (baseDim Dimensionless, v)

canonical_unit :: Dimension -> Unit
canonical_unit dim = conversion_ratio dim 1

conversion_ratio :: Dimension -> Value -> Unit
conversion_ratio dim r = complex_conversion dim (*r)

complex_conversion :: Dimension -> (Value -> Value) -> Unit
complex_conversion dim f = \v -> (dim, f v)

ofDim :: Unit -> Dimension
ofDim u = fst $ u 1

-- Unit and Quantity manipulation

applyPrefix :: Prefix -> Unit -> Unit
applyPrefix p u = \v -> second (*p) (u v)

applyModifier :: Modifier -> Value -> Value
applyModifier m v = v * m

mkQu :: Value -> Unit -> Quantity
mkQu v u = u v   -- haha

quIn :: Quantity -> Unit -> Maybe Quantity
quIn (d, v) u =
  let (du, v') = u v
  in if d /= du then Nothing
  else Just (d, v')


-- Unit operators

{-
infixl 8 #^
(#^) :: Unit -> Unit -> Maybe Unit
(d, v) #^ (dp, p) = if dp /= baseDim Dimensionless then Nothing else
  if denominator p == 1 then Just (d |^| p, v ^ numerator p)
  else Just (d |^| p, toRational (fromRational v ** fromRational p))
-}

infixl 7 #*
(#*) :: Unit -> Unit -> Unit
u1 #* u2 =
  \v ->
    let (d1, v1) = u1 v
        (d2, v2) = u2 1
    in (d1 |*| d2, v1 * v2)


infixl 7 #/
(#/) :: Unit -> Unit -> Unit
u1 #/ u2 =
  \v ->
    let (d1, v1) = u1 v
        (d2, v2) = u2 1
    in (d1 |/| d2, v1 / v2)


-- Quantity operators

infixl 7 %*
(%*) :: Quantity -> Quantity -> Quantity
u1 %* u2 = ubimap (ubimap ((|*|), (*)) u1) u2 where
  ubimap = uncurry bimap

infixl 7 %/
(%/) :: Quantity -> Quantity -> Quantity
u1 %/ u2 = ubimap (ubimap ((|/|), (/)) u1) u2 where
  ubimap = uncurry bimap

infix 6 %+
(%+) :: Quantity -> Quantity -> Maybe Quantity
(d1, v1) %+ (d2, v2) = if d1 /= d2 then Nothing
                       else Just (d1, v1 + v2)

infix 6 %-
(%-) :: Quantity -> Quantity -> Maybe Quantity
(d1, v1) %- (d2, v2) = if d1 /= d2 then Nothing
                       else Just (d1, v1 - v2)
