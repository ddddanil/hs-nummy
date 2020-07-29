{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nummy.Metrology.Base (
  Prefix, Modifier, Label
, Value(..), valueF, (^^^)
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.Ratio (approxRational)
import qualified Text.PrettyPrint.Leijen as PP


type Prefix = (Value, Label)
type Modifier = Value
type Label = String


-- Value type

newtype Value = Value { value :: Rational } deriving (Eq, Show, Ord, Num, Fractional, Real, RealFrac)

instance PP.Pretty Value where
  pretty (Value v) =
    if denominator v == 1
      then PP.pretty $ numerator v
      else PP.pretty ( fromRational v :: Double )

infixr 8 ^^^
(^^^) :: Value -> Value -> Value
(^^^) (Value v1) (Value v2) =
  if denominator v2 == 1
    then Value $ v1 ^^ (numerator v2)
    else valueF ((fromRational v1 ** fromRational v2) :: Double)

valueF :: (RealFrac f) => f -> Value
valueF f = Value $ approxRational f epsilon
  where epsilon = 0.000001
