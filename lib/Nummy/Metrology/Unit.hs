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
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.Tuple.Extra hiding (first, second)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology.Base as B
import Nummy.Metrology.Dimension as D


-- Class

-- | Unit typeclass
class (Eq a, PP.Pretty a) => CUnit a where
  -- | Convert a value in a given unit into SI
  --
  -- >>> toSi foot 1
  -- 0.3048
  toSi      :: a         -- ^ CUnit /(self)/
            -> Value     -- ^ input value
            -> Value     -- ^ value in SI

  -- | Convert a value in SI into the given unit
  --
  -- >>> fromSi minute 120
  -- 2
  fromSi    :: a         -- ^ CUnit /(self)/
            -> Value     -- ^ input value in SI
            -> Value     -- ^ value in the given unit

  -- | Get the dimension of a unit
  dimension :: a         -- ^ CUnit /(self)/
            -> Dimension -- ^ Dimension of the unit


-- General wrapper

-- | Box-wrapper for any 'CUnit' instance
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

-- | Combine a 'CUnit' with a 'B.Prefix'
infixr 9 -|
data (-|) p u where
  Prefix' :: (CUnit a) => Prefix -> a -> (Prefix -| a)

instance (Eq a) => Eq (Prefix -| a) where
  (Prefix' p1 u1) == (Prefix' p2 u2) =
    p1 == p2 && u1 == u2

instance (PP.Pretty a) => PP.Pretty (Prefix -| a) where
  pretty (Prefix' p x) = PP.pretty p <> PP.pretty x

instance (CUnit a) => CUnit (Prefix -| a) where
  toSi   (Prefix' (Prefix (p, _)) x) = \v -> p * toSi x v
  fromSi (Prefix' (Prefix (p, _)) x) = \v -> fromSi x v / p
  dimension (Prefix' _ x) = dimension x

-- | Attach a 'B.Prefix' to a 'Unit'
--
-- >  kilo -| meter
(-|) :: Prefix -> Unit -> Unit
p -| (Unit u) = Unit $ Prefix' p u


-- Power instance

-- | Combine a 'CUnit' with a 'Value'
infixl 8 #^
data (#^) u p where
  Power :: (CUnit a) => a -> Value -> (a #^ Value)

instance (Eq a) => Eq (a #^ Value) where
  (Power u1 p1) == (Power u2 p2) =
    u1 == u2 && p1 == p2

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

-- | Raise 'Unit' to a power
--
-- >  meter #^ 2
(#^) :: Unit -> Value -> Unit
(Unit u) #^ p = Unit $ Power u p


-- Multiplication instance

-- | Combine two 'CUnit's by multiplying them
infixl 7 #*
data (#*) u1 u2 = Mult u1 u2 deriving Eq

instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (a #* b) where
  pretty (Mult u1 u2) = PP.pretty u1 PP.<+> PP.pretty u2

instance (CUnit a, CUnit b) => CUnit (a #* b) where
  toSi (Mult u1 u2) = toSi u1 . toSi u2
  fromSi (Mult u1 u2) = fromSi u1 . fromSi u2
  dimension (Mult u1 u2) = dimension u1 |*| dimension u2

-- | Multiply two 'Unit's
--
-- >  gram #* second
(#*) :: Unit -> Unit -> Unit
(Unit u1) #* (Unit u2) = Unit $ Mult u1 u2


-- Division instance

-- | Combine two 'CUnit's by dividing them
infixl 7 #/
data (#/) u1 u2 = Div u1 u2 deriving Eq

instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (a #/ b) where
  pretty (Div u1 u2) = PP.pretty u1 <> PP.char '/' <> PP.pretty u2

instance (CUnit a, CUnit b) => CUnit (a #/ b) where
  toSi (Div u1 u2) = \v -> v * (toSi u1 1 / toSi u2 1)
  fromSi (Div u1 u2) = \v -> v * (fromSi u1 1 / fromSi u2 1)
  dimension (Div u1 u2) = dimension u1 |/| dimension u2

-- | Divide two 'Unit's
--
-- >  meter #/ second
(#/) :: Unit -> Unit -> Unit
(Unit u1) #/ (Unit u2) = Unit $ Div u1 u2


-- Basic constructors

-- | Provide functions to convert to SI and back
complex_conversion :: Dimension -> Label -> (Value -> Value) -> (Value -> Value) -> Unit
complex_conversion d l f g = Unit $ BaseUnit (f, g, d, l)

-- | Unit is proportional to SI
conversion_ratio :: Dimension -> Label -> Value -> Unit
conversion_ratio d l r = complex_conversion d l (*r) (/r)

-- | Unit is a canonical SI unit
canonical_unit :: Dimension -> Label -> Unit
canonical_unit d l = conversion_ratio d l 1

-- | Unit of a scalar value
dimless_unit :: Unit
dimless_unit = Unit $ Dimless 1
