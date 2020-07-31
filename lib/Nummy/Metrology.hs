{-# LANGUAGE ExplicitNamespaces #-}

{-|
Module        : Nummy.Metrology
Description   : Types for dimensions, units and quantities geared for arithmetics
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Metrology (
-- Types
  B.Label
, B.Prefix
-- * Value
, B.Value, B.valueF, B.valueI, (B.^^^)
-- * Dimension
, D.Dimension
-- ** Operators on dimensions
, (D.|^|)
, (D.|*|)
, (D.|/|)
-- * Unit
, U.Unit
, U.toSi
, U.fromSi
, U.dimension
-- ** Operators on units
-- *** Prefix
, (U.-|)
-- *** Power
, (U.#^)
-- *** Product
, (U.#*)
-- *** Quotient
, (U.#/)
-- * Quantity
, Qu.Quantity(..)
, Qu.dimOfQu, (Qu.%#), (Qu.%<|)
-- ** Operators on quantities
, (Qu.%^), (Qu.%*), (Qu.%/), (Qu.%+), (Qu.%-)
) where

import qualified Nummy.Metrology.Base as B
import qualified Nummy.Metrology.Unit as U
import qualified Nummy.Metrology.Dimension as D
import qualified Nummy.Metrology.Definitions as Def
import qualified Nummy.Metrology.Quantity as Qu
