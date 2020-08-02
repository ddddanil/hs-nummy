module Nummy.Metrology.Unit (
  Unit
, convert, dimension
, (-|), (#^), (#*), (#/)
, complex_conversion, conversion_ratio, canonical_unit, scalar_unit
) where

import Nummy.Prelude hiding (Prefix)
import GHC.Show as S
import Data.List (intersect, (\\))
import Data.Text.Prettyprint.Doc

import Nummy.Base
import Nummy.Metrology.Dimension
import Nummy.Metrology.Prefix

-- | Datatype representing a unit
data Unit
  = ScalarUnit Value
  | BaseUnit (Value -> Value, Value -> Value, Dimension, Label)
  | PrefixUnit Prefix Unit
  | PowerUnit Unit Value
  | MultUnit Unit Unit
  | DivUnit Unit Unit

-- | Get a function to convert a value from one unit into another
convert :: Unit -> Unit -> (Value -> Value)
convert u1 u2 = fromSi u2 . toSi u1

-- | Get a function that converts a value represented in that unit into SI
toSi :: Unit -> (Value -> Value)
toSi (ScalarUnit x) = \v -> v * x
toSi (BaseUnit (f, _, _, _)) = f
toSi (PrefixUnit (Prefix (p, _)) x) = \v -> p * toSi x v
toSi (PowerUnit u p) = \v -> v * (toSi u 1 ^^^ p)
toSi (MultUnit u1 u2) = toSi u1 . toSi u2
toSi (DivUnit u1 u2) = \v -> v * (toSi u1 1 / toSi u2 1)

-- | Get a function that converts a value in SI into a given unit
fromSi :: Unit -> (Value -> Value)
fromSi (ScalarUnit x) = \v -> v / x
fromSi (BaseUnit (_, g, _, _)) = g
fromSi (PrefixUnit (Prefix (p, _)) x) = \v -> fromSi x v / p
fromSi (PowerUnit u p) = \v -> v * (fromSi u 1 ^^^ p)
fromSi (MultUnit u1 u2) = fromSi u1 . fromSi u2
fromSi (DivUnit u1 u2) = \v -> v * (fromSi u1 1 / fromSi u2 1)

-- | Get the dimension of a unit
dimension :: Unit -> Dimension
dimension (ScalarUnit _) = scalar
dimension (BaseUnit (_, _, d, _)) = d
dimension (PrefixUnit _ x) = dimension x
dimension (PowerUnit u p) = dimension u |^| p
dimension (MultUnit u1 u2) = dimension u1 |*| dimension u2
dimension (DivUnit u1 u2) = dimension u1 |/| dimension u2

-- | Simplify the internal structure of a unit (performance drain)
simplify :: Unit -> Unit
-- Propagate powers into multiplication
simplify (PowerUnit (MultUnit u1 u2) v) = simplify $ MultUnit (simplify (PowerUnit u1 v)) (simplify (PowerUnit u2 v))
-- Propagate powers into division
simplify (PowerUnit (DivUnit u1 u2) v) = simplify $ DivUnit (simplify (PowerUnit u1 v)) (simplify (PowerUnit u2 v))
-- Assoc multiplication to the right
simplify (MultUnit (MultUnit u1 u2) u3) = simplify $ MultUnit (simplify u1) (simplify (MultUnit u2 u3))
-- Propagate multiplication into the numerator of division
simplify (MultUnit un (DivUnit u1 u2)) = simplify $ DivUnit (simplify (MultUnit un u1)) (simplify u2)
simplify (MultUnit (DivUnit u1 u2) un) = simplify $ DivUnit (simplify (MultUnit un u1)) (simplify u2)
-- Remove Scalar 1 from multiplication
simplify (MultUnit (ScalarUnit 1) u2) = simplify u2
-- Remove Scalar 1 from division
simplify (DivUnit u1 (ScalarUnit 1)) = simplify u1
-- Remove double division
simplify (DivUnit un (DivUnit u1 u2)) = simplify $ DivUnit (simplify (MultUnit un u2)) (simplify u1)
-- Remove repeating terms
simplify (DivUnit num den) =
  case new_den of
    (ScalarUnit 1) -> new_num
    _ -> DivUnit new_num new_den
  where
    mult_unfold :: Unit -> [Unit]
    mult_unfold (MultUnit u rest) = u : mult_unfold rest
    mult_unfold a = [a]
    mult_fold :: Unit -> [Unit] -> Unit
    mult_fold u (n:rest) = mult_fold (u #* n) rest
    mult_fold u [] = u
    nums = mult_unfold num
    dens = mult_unfold den
    repeating = intersect nums dens
    new_num = simplify $ mult_fold scalar_unit $ nums \\ repeating
    new_den = simplify $ mult_fold scalar_unit $ dens \\ repeating
-- catch-all
simplify x = x


instance Show Unit where
  show (ScalarUnit v) = "ScalarUnit " ++ S.show v
  show (BaseUnit (f, g, d, l)) = "BaseUnit(" ++ S.show (f 1) ++ "," ++ S.show (g 1)
                                          ++ "," ++ S.show d ++ "," ++ S.show l ++ ")"
  show (PrefixUnit p u) = "PrefixUnit(" ++ S.show p ++ " " ++ S.show u ++ ")"
  show (PowerUnit a v) = "PowerUnit(" ++ S.show a ++ " " ++ S.show v ++ ")"
  show (MultUnit u1 u2) = "MultUnit(" ++ S.show u1 ++ " " ++ S.show u2 ++ ")"
  show (DivUnit u1 u2) = "DivUnit(" ++ S.show u1 ++ " " ++ S.show u2 ++ ")"


instance Pretty Unit where
  pretty (ScalarUnit v) =
    if v == 1
      then mempty
      else pretty v
  pretty (BaseUnit (_, _, _, l)) = pretty l
  pretty (PrefixUnit p x) = pretty p <> pretty x
  pretty (PowerUnit u p) = pretty u <> pretty_power p where
    pretty_power 1 = mempty
    pretty_power 2 = pretty '²'
    pretty_power 3 = pretty '³'
    pretty_power x = pretty '^' <> pretty x
  pretty (MultUnit u1 u2) = pretty u1 <+> pretty u2
  pretty (DivUnit u1 u2) = pretty u1 <> pretty '/' <> pretty u2

instance Eq Unit where
  -- Structural equality
  (ScalarUnit v1) == (ScalarUnit v2) =
    v1 == v2

  BaseUnit (f1, g1, d1, l1) == BaseUnit (f2, g2, d2, l2) =
    f1 1 == f2 1 && g1 1 == g2 1 && d1 == d2 && l1 == l2

  (PrefixUnit p1 u1) == (PrefixUnit p2 u2) =
    p1 == p2 && u1 == u2

  (PowerUnit u1 p1) == (PowerUnit u2 p2) =
    u1 == u2 && p1 == p2

  (MultUnit u1 u2) == (MultUnit u3 u4) =
    u1 == u3 && u2 == u4 ||
    u1 == u4 && u2 == u3

  (DivUnit u1 u2) == (DivUnit u3 u4) =
    u1 == u3 && u2 == u4

  -- Effective equality
  u1 == u2 =
    toSi u1 1 == toSi u2 1 &&
    fromSi u1 1 == fromSi u2 1 &&
    dimension u1 == dimension u2



-- | Attach a 'B.Prefix' to a 'Unit'
--
-- >  kilo -| meter
infixr 9 -|
(-|) :: Prefix -> Unit -> Unit
p -| u = PrefixUnit p u


-- | Raise 'Unit' to a power
--
-- >  meter #^ 2
infixl 8 #^
(#^) :: Unit -> Value -> Unit
u #^ p = simplify $ PowerUnit u p


-- | Multiply two 'Unit's
--
-- >  gram #* second
infixl 7 #*
(#*) :: Unit -> Unit -> Unit
u1 #* u2 = simplify $ MultUnit u1 u2


-- | Divide two 'Unit's
--
-- >  meter #/ second
infixl 7 #/
(#/) :: Unit -> Unit -> Unit
u1 #/ u2 = simplify $ DivUnit u1 u2


-- Basic constructors

-- | Provide functions to convert to SI and back
complex_conversion :: Dimension -> Label -> (Value -> Value) -> (Value -> Value) -> Unit
complex_conversion d l f g = BaseUnit (f, g, d, l)

-- | Unit is proportional to SI
conversion_ratio :: Dimension -> Label -> Value -> Unit
conversion_ratio d l r = complex_conversion d l (*r) (/r)

-- | Unit is a canonical SI unit
canonical_unit :: Dimension -> Label -> Unit
canonical_unit d l = conversion_ratio d l 1

-- | Unit of a scalar value
scalar_unit :: Unit
scalar_unit = ScalarUnit 1
