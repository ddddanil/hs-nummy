{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Nummy.Metrology.Unit (
  Unit(..)
, BaseUnit
, type (-|), (-|)
, type (#^), (#^)
, type (#*), (#*)
, type (#/), (#/)
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.Tuple.Extra hiding (first, second)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Base
import Nummy.Metrology.Dimension as D


-- Class

class (Eq a, PP.Pretty a) => Unit a where
  toSi :: a -> Value -> Value
  fromSi :: a -> Value -> Value
  dimension :: a -> Dimension


-- Base instance

newtype BaseUnit = BaseUnit (Value -> Value, Value -> Value, Dimension, Label)

instance Eq BaseUnit where
  BaseUnit (f1, g1, d1, l1) == BaseUnit (f2, g2, d2, l2) =
    f1 1 == f2 1 && g1 1 == g2 1 && d1 == d2 && l1 == l2

instance PP.Pretty BaseUnit where
  pretty (BaseUnit (_, _, _, l)) = PP.text l

instance Unit BaseUnit where
  toSi (BaseUnit (f, _, _, _)) = f
  fromSi (BaseUnit (_, g, _, _)) = g
  dimension (BaseUnit (_, _, d, _)) = d


-- Prefix instance

infixr 8 -|
data (-|) p u = Prefix p u deriving Eq
(-|) = Prefix

instance (PP.Pretty a) => PP.Pretty (Prefix -| a) where
  pretty (Prefix (_, pl) x) = PP.text pl <> PP.pretty x

instance (Unit a) => Unit (Prefix -| a) where
  toSi   (Prefix (p, _) x) = \v -> p * toSi x v
  fromSi (Prefix (p, _) x) = \v -> fromSi x v / p
  dimension (Prefix _ x) = dimension x


-- Power instance

infixl 8 #^
data (#^) u p = Power u p deriving Eq
(#^) = Power

instance (PP.Pretty a) => PP.Pretty (a #^ Value) where
  pretty (Power u p) = PP.pretty u <> pretty_power p where
    pretty_power :: Value -> PP.Doc
    pretty_power 1 = PP.empty
    pretty_power 2 = PP.char '²'
    pretty_power 3 = PP.char '³'
    pretty_power x = PP.char '^' <> PP.pretty x

instance (Unit a) => Unit (a #^ Value) where
  toSi (Power u p) = \v -> v * (toSi u 1 ^^^ p)
  fromSi (Power u p) = \v -> v * (fromSi u 1 ^^^ p)
  dimension (Power u p) = dimension u |^| p

-- Multiplication instance

infixl 7 #*
data (#*) u1 u2 = Mult u1 u2 deriving Eq
(#*) = Mult

instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (a #* b) where
  pretty (Mult u1 u2) = PP.pretty u1 PP.<+> PP.pretty u2

instance (Unit a, Unit b) => Unit (a #* b) where
  toSi (Mult u1 u2) = toSi u1 . toSi u2
  fromSi (Mult u1 u2) = fromSi u1 . fromSi u2
  dimension (Mult u1 u2) = dimension u1 |*| dimension u2


-- Division instance

infixl 7 #/
data (#/) u1 u2 = Div u1 u2 deriving Eq
(#/) = Div

instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (a #/ b) where
  pretty (Div u1 u2) = PP.pretty u1 <> PP.char '/' <> PP.pretty u2

instance (Unit a, Unit b) => Unit (a #/ b) where
  toSi (Div u1 u2) = \v -> v * (toSi u1 1 / toSi u2 1)
  fromSi (Div u1 u2) = \v -> v * (fromSi u1 1 / fromSi u2 1)
  dimension (Div u1 u2) = dimension u1 |/| dimension u2


-- Basic constructors

complex_conversion :: Dimension -> Label -> (Value -> Value) -> (Value -> Value) -> BaseUnit
complex_conversion d l f g = BaseUnit $ (f, g, d, l)

conversion_ratio :: Dimension -> Label -> Value -> BaseUnit
conversion_ratio d l r = complex_conversion d l (*r) (/r)

canonical_unit :: Dimension -> Label -> BaseUnit
canonical_unit d l = conversion_ratio d l 1
