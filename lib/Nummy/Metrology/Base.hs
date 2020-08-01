{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nummy.Metrology.Base (
  Label
, Prefix(..), PrefixType(..)
, ReadUnit, runReadUnit
, Value(..), valueF, valueI, (^^^)
) where

import Nummy.Prelude hiding (Prefix)
import Data.Ratio (approxRational)
import Data.Text.Prettyprint.Doc
import Nummy.Cache


-- Types

-- | Preferred string-like datatype
type Label = Text

type ReadUnit = ReaderT CurrencyCache IO

runReadUnit :: ReaderT CurrencyCache IO b -> IO b
runReadUnit m = newCurrencyCache >>= runReaderT (m)


-- Prefix

-- | Representation of a unit prefix
newtype Prefix = Prefix (Value, Label) -- ^ (prefix multiplier, prefix name)
  deriving (Show, Eq)

data PrefixType = PrefixBelowOne | PrefixBinary | PrefixAboveOne
  deriving (Eq, Show, Ord, Enum)

instance Pretty Prefix where
  pretty (Prefix (_, l)) = pretty l


-- Value

-- | Boxed 'Rational' value with additional properties
newtype Value =
  Value { value :: Rational -- ^ Boxed value
        }
  deriving (Eq, Show, Ord, Num, Fractional, Real, RealFrac, Read)

instance Pretty Value where
  pretty (Value v) =
    if denominator v == 1
      then pretty $ numerator v
      else pretty ( fromRational v :: Double )

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

-- | Generate a 'Value' from an 'Integer'
valueI :: Integer -> Value
valueI x = Value $ x % 1
