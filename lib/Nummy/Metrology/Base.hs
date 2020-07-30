{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nummy.Metrology.Base (
  Prefix(..), Label
, Value(..), valueF, valueI, (^^^)
) where

import Nummy.Prelude hiding (Prefix)
import Data.Ratio (approxRational)
import qualified Text.PrettyPrint.Leijen as PP


-- Types

-- | Preferred string-like datatype
type Label = String


-- | Representation of a unit prefix
newtype Prefix = Prefix (Value, Label) -- ^ (prefix multiplier, prefix name)
  deriving (Show, Eq)

instance PP.Pretty Prefix where
  pretty (Prefix (_, l)) = PP.text l


-- | Boxed 'Rational' value with additional properties
newtype Value =
  Value { value :: Rational -- ^ Boxed value
        }
  deriving (Eq, Show, Ord, Num, Fractional, Real, RealFrac, Read)

instance PP.Pretty Value where
  pretty (Value v) =
    if denominator v == 1
      then PP.pretty $ numerator v
      else PP.pretty ( fromRational v :: Double )

-- | Raise one 'Value' to the power of another
-- Optimises for whole numbers, falls back on doubles
infixr 8 ^^^
(^^^) :: Value -> Value -> Value
(^^^) (Value v1) (Value v2) =
  if denominator v2 == 1
    then Value $ v1 ^^ (numerator v2)
    else valueF ((fromRational v1 ** fromRational v2) :: Double)

-- | Approximate a 'RealFrac' number into 'Value' with precision /epsilon/ = 0.000001
valueF :: (RealFrac f) => f -> Value
valueF f = Value $ approxRational f epsilon
  where epsilon = 0.000001

valueI :: Integer -> Value
valueI x = Value $ x % 1
