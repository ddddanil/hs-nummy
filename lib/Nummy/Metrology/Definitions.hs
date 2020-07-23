{-# LANGUAGE TemplateHaskellQuotes #-}

module Nummy.Metrology.Definitions (
  symbol_table,
  Length
) where

import Protolude hiding (Prefix)
import Data.Maybe (fromJust)
import Data.List (lookup)
import Language.Haskell.TH as TH

import Nummy.Metrology.Dimension

symbol_table :: [ (Name, [Unit]) ]
symbol_table =
  [ (''Length,
      [ ("m", 1)
      , ("ft", 0.3048)
      ]
    )
  , (''Prefix,
      [ ("k", 1000)
      , ("m", 0.001)
      ]
    )
  ]

data Length = Length
instance Dimension Length where
  units _ = fromJust $ lookup ''Length symbol_table


data Prefix = Prefix
-- instance Dimension Prefix where
--   units _ = fromJust $ lookup ''Prefix symbol_table
