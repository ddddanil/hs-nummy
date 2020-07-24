module Nummy.Metrology.Unit (
  Unit, Quantity
, dimlessUnit
, applyPrefix, applyModifier
, mkQu, quIn
, (#*), (#/), (#+), (#-)
) where

import Protolude hiding (Prefix)
import Data.String (String)
import GHC.Err (error)

import Nummy.Metrology.Dimension

type Unit = (Dimension, Value)  -- (Dimension, conversion to SI)
type Quantity = (Dimension, Value)


dimlessUnit :: Unit
dimlessUnit = (baseDim Dimensionless, 1)

-- Unit and Quantity manipulation

applyPrefix :: Unit -> Unit -> Unit
applyPrefix (dp, p) (d, value) = if dp == baseDim Prefix then (d, p * value)
                                 else error "You must apply a prefix"

applyModifier :: Unit -> Value -> Value
applyModifier m v = v * snd m

mkQu :: Value -> Unit -> Quantity
mkQu v (d, u) = (d, v * u)


quIn :: Quantity -> Unit -> Maybe Quantity
quIn (d1, v1) (d2, v2) = if d1 /= d2 then Nothing
                         else Just (d1, v1 / v2)


-- Operators

infixl 7 #*
(#*) :: Unit -> Unit -> Unit
u1 #* u2 = ubimap (ubimap ((|*|), (*)) u1) u2 where
  ubimap = uncurry bimap

infixl 7 #/
(#/) :: Unit -> Unit -> Unit
u1 #/ u2 = ubimap (ubimap ((|/|), (/)) u1) u2 where
  ubimap = uncurry bimap

infix 6 #+
(#+) :: Unit -> Unit -> Maybe Unit
(d1, v1) #+ (d2, v2) = if d1 /= d2 then Nothing
                       else Just (d1, v1 + v2)

infix 6 #-
(#-) :: Unit -> Unit -> Maybe Unit
(d1, v1) #- (d2, v2) = if d1 /= d2 then Nothing
                       else Just (d1, v1 - v2)
