{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Nummy.Metrology.Unit (
  Unit, CUnit(..)
, type (-|), (-|)
, type (#^), (#^)
, type (#*), (#*)
, type (#/), (#/)
, complex_conversion, conversion_ratio, canonical_unit, dimless_unit
, unitIsDimless
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.Tuple.Extra hiding (first, second)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Base
import Nummy.Metrology.Dimension as D


-- Class

class (Eq a, PP.Pretty a) => CUnit a where
  toSi :: a -> Value -> Value
  fromSi :: a -> Value -> Value
  dimension :: a -> Dimension


-- General wrapper

data Unit where
  Unit :: CUnit a => a -> Unit

instance PP.Pretty Unit where
  pretty (Unit u) = PP.pretty u

instance Eq Unit where
  (Unit u1) == (Unit u2) =
    toSi u1 1 == toSi u2 1 &&
    fromSi u1 1 == fromSi u2 1 &&
    dimension u1 == dimension u2

instance CUnit Unit where
  toSi (Unit u) = toSi u
  fromSi (Unit u) = fromSi u
  dimension (Unit u) = dimension u


-- Dimless instance

data Dimless = Dimless Value deriving Eq

instance PP.Pretty Dimless where
  pretty (Dimless v) =
    if v == 1
      then PP.empty
      else PP.pretty v

instance CUnit Dimless where
  toSi (Dimless x) = \v -> v * x
  fromSi (Dimless x) = \v -> v / x
  dimension (Dimless _) = dimless


-- Base instance

newtype BaseUnit = BaseUnit (Value -> Value, Value -> Value, Dimension, Label)

instance Eq BaseUnit where
  BaseUnit (f1, g1, d1, l1) == BaseUnit (f2, g2, d2, l2) =
    f1 1 == f2 1 && g1 1 == g2 1 && d1 == d2 && l1 == l2

instance PP.Pretty BaseUnit where
  pretty (BaseUnit (_, _, _, l)) = PP.text l

instance CUnit BaseUnit where
  toSi (BaseUnit (f, _, _, _)) = f
  fromSi (BaseUnit (_, g, _, _)) = g
  dimension (BaseUnit (_, _, d, _)) = d


-- Prefix instance

infixr 9 -|
data (-|) p u = Prefix p u deriving Eq
(-|) :: Prefix -> Unit -> Unit
p -| (Unit u) = Unit $ Prefix p u

instance (PP.Pretty a) => PP.Pretty (Prefix -| a) where
  pretty (Prefix (_, pl) x) = PP.text pl <> PP.pretty x

instance (CUnit a) => CUnit (Prefix -| a) where
  toSi   (Prefix (p, _) x) = \v -> p * toSi x v
  fromSi (Prefix (p, _) x) = \v -> fromSi x v / p
  dimension (Prefix _ x) = dimension x


-- Power instance

infixl 8 #^
data (#^) u p = Power u p deriving Eq
(#^) :: Unit -> Value -> Unit
(Unit u) #^ p = Unit $ Power u p

instance (PP.Pretty a) => PP.Pretty (a #^ Value) where
  pretty (Power u p) = PP.pretty u <> pretty_power p where
    pretty_power :: Value -> PP.Doc
    pretty_power 1 = PP.empty
    pretty_power 2 = PP.char '²'
    pretty_power 3 = PP.char '³'
    pretty_power x = PP.char '^' <> PP.pretty x

instance (CUnit a) => CUnit (a #^ Value) where
  toSi (Power u p) = \v -> v * (toSi u 1 ^^^ p)
  fromSi (Power u p) = \v -> v * (fromSi u 1 ^^^ p)
  dimension (Power u p) = dimension u |^| p


-- Multiplication instance

infixl 7 #*
data (#*) u1 u2 = Mult u1 u2 deriving Eq
(#*) :: Unit -> Unit -> Unit
(Unit u1) #* (Unit u2) = Unit $ Mult u1 u2

instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (a #* b) where
  pretty (Mult u1 u2) = PP.pretty u1 PP.<+> PP.pretty u2

instance (CUnit a, CUnit b) => CUnit (a #* b) where
  toSi (Mult u1 u2) = toSi u1 . toSi u2
  fromSi (Mult u1 u2) = fromSi u1 . fromSi u2
  dimension (Mult u1 u2) = dimension u1 |*| dimension u2


-- Division instance

infixl 7 #/
data (#/) u1 u2 = Div u1 u2 deriving Eq
(#/) :: Unit -> Unit -> Unit
(Unit u1) #/ (Unit u2) = Unit $ Div u1 u2

instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (a #/ b) where
  pretty (Div u1 u2) = PP.pretty u1 <> PP.char '/' <> PP.pretty u2

instance (CUnit a, CUnit b) => CUnit (a #/ b) where
  toSi (Div u1 u2) = \v -> v * (toSi u1 1 / toSi u2 1)
  fromSi (Div u1 u2) = \v -> v * (fromSi u1 1 / fromSi u2 1)
  dimension (Div u1 u2) = dimension u1 |/| dimension u2


-- Basic constructors

complex_conversion :: Dimension -> Label -> (Value -> Value) -> (Value -> Value) -> Unit
complex_conversion d l f g = Unit $ BaseUnit (f, g, d, l)

conversion_ratio :: Dimension -> Label -> Value -> Unit
conversion_ratio d l r = complex_conversion d l (*r) (/r)

canonical_unit :: Dimension -> Label -> Unit
canonical_unit d l = conversion_ratio d l 1

dimless_unit :: Unit
dimless_unit = Unit $ Dimless 1


-- Helpers

unitIsDimless :: Unit -> Bool
unitIsDimless u = dimension u == dimless
