module Nummy.Metrology.Unit (
  Unit,  Prefix, Modifier
, dimlessUnit, dimlessCoeff, conversion_ratio, complex_conversion, canonical_unit
, applyPrefix, applyModifier
, dimOfUnit
, convert
, Nummy.Metrology.Unit.isDimless
, (#^), (#!^), (#*), (#/)
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.Tuple.Extra hiding (first, second)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Dimension as D


type Prefix = Value
type Modifier = Value


-- Type

newtype Unit = Unit (Value -> Value, Value -> Value, Dimension)
-- conversion functions  ^ - unit -> SI   ^ - SI -> Unit

instance PP.Pretty Unit where
  pretty (Unit u) = PP.pretty (thd3 u)

instance Eq Unit where
  (Unit u1) == (Unit u2) =
    fst3 u1 1 == fst3 u2 1 &&
    snd3 u1 1 == snd3 u2 1 &&
    thd3 u1   == thd3 u2

isDimless :: Unit -> Bool
isDimless (Unit (_, _, d)) = d == dimless

dimlessUnit :: Unit
dimlessUnit = Unit $ (\v -> v, \v -> v, dimless)

-- rename
dimlessCoeff :: Value -> Unit
dimlessCoeff c = conversion_ratio dimless c

canonical_unit :: Dimension -> Unit
canonical_unit dim = conversion_ratio dim 1

conversion_ratio :: Dimension -> Value -> Unit
conversion_ratio dim r = complex_conversion dim (*r) (/r)

complex_conversion :: Dimension -> (Value -> Value) -> (Value -> Value) -> Unit
complex_conversion dim f g = Unit $ (f, g, dim)

dimOfUnit :: Unit -> Dimension
dimOfUnit (Unit u) = thd3 u


-- Unit manipulation

applyPrefix :: Prefix -> Unit -> Unit
applyPrefix p (Unit u) = Unit $ ((*p) . (fst3 u), (/p) . (snd3 u), thd3 u)

applyModifier :: Modifier -> Value -> Value
applyModifier m v = v * m

-- convert the value into SI and back into another unit
convert :: Unit -> Unit -> (Value -> Value)
convert (Unit u1) (Unit u2) = (snd3 u2) . (fst3 u1)


-- Unit operators

infix 8 #!^
(#!^) :: Unit -> Unit -> Maybe Unit
u1 #!^ (Unit (i, o, d)) =
  let v = i 1
  in if D.isDimless d
     then Just (u1 #^ v)
     else Nothing

infixl 8 #^
(#^) :: Unit -> Value -> Unit
(Unit (o, i, d)) #^ p =
  Unit $
    ( \v -> v * (pow (o 1) p)
    , \v -> v * (pow (i 1) (1/p))
    , d |^| p
    )
  where
    pow :: Value -> Value -> Value
    pow v p =
      if denominator p == 1
      then v ^ (numerator p)
      else toRational $ fromRational v ** fromRational p

infixl 7 #*
(#*) :: Unit -> Unit -> Unit
(Unit (o1, i1, d1)) #* (Unit (o2, i2, d2)) =
  Unit $
    ( \v -> v * (o1 1) * (o2 1)
    , \v -> v * (i1 1) * (i2 1)
    , d1 |*| d2
    )

infixl 7 #/
(#/) :: Unit -> Unit -> Unit
(Unit (o1, i1, d1)) #/ (Unit (o2, i2, d2)) =
  Unit $
    ( \v -> v * (o1 1) / (o2 1)
    , \v -> v * (i1 1) / (i2 1)
    , d1 |/| d2
    )

