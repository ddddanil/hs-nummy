{-# LANGUAGE ExplicitNamespaces #-}

{-|
Module        : Nummy.Metrology
Description   : Types for dimensions, units and quantities geared for arithmetics
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Metrology (
-- Types
-- * Dimension
  D.Dimension
-- ** Operators on dimensions
, (D.|^|)
, (D.|*|)
, (D.|/|)
-- * Prefix
, P.Prefix
-- * Unit
, U.Unit
, U.convert
, U.dimension
-- ** Operators on units
, (U.-|)
, (U.#^)
, (U.#*)
, (U.#/)
-- * Quantity
, Qu.Quantity
, Qu.dimOfQu, (Qu.%#), (Qu.%<|)
-- ** Operators on quantities
, (Qu.%^), (Qu.%*), (Qu.%/), (Qu.%+), (Qu.%-)
) where

import qualified Nummy.Metrology.Prefix as P
import qualified Nummy.Metrology.Unit as U
import qualified Nummy.Metrology.Dimension as D
import qualified Nummy.Metrology.Quantity as Qu
