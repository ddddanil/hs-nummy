{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Nummy.Metrology.Dimension (
  Dimension (units), Unit
, Dimensionless (Dimensionless)
, (:*), (:/), (:+), (:-) --, (:^)
) where

import Protolude

type Unit = ([Char], Rational)
type Quantity = (Rational, Dimension)

class Dimension dim where
  units :: dim -> [Unit]

instance Dimension Dimensionless where
  units _ = []

-- instance (Dimension dim, Floating pow) => Dimension (dim :^ pow) where
--   units :: (Dimension dim, Floating pow) => (dim :^ pow) -> [Unit]
--   units (dim :^ pow) = map (second (** pow)) $ units dim

instance (Dimension d1, Dimension d2) => Dimension (d1 :* d2) where
  units (d1 :* d2) = intermingle ((++),(*)) (units d1) (units d2)

instance (Dimension d1, Dimension d2) => Dimension (d1 :/ d2) where
  units (d1 :/ d2) = intermingle ((++),(/)) (units d1) (units d2)

data Dimensionless = Dimensionless

-- infixr 8 :^
-- data (:^) dim pow = dim :^ pow

infixr 7 :*
data (:*) d1 d2 = d1 :* d2

infixr 7 :/
data (:/) d1 d2 = d1 :/ d2

infixr 6 :+
data (:+) d1 d2 = d1 :+ d2

infixr 6 :-
data (:-) d1 d2 = d1 :- d2

simplify :: (Dimension d1, Dimension d2) => d1 -> d2
simplify = undefined

intermingle :: (a -> a -> b, c -> c -> d) -> [(a, c)] -> [(a, c)] -> [(b, d)]
intermingle bf xs ys = liftA2 ubimap (ubimap bf <$> xs) ys where
  ubimap = uncurry bimap

